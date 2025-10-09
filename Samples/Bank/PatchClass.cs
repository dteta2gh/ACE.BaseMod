
using ACE.Server.Network;
using ACE.Server.WorldObjects;
using Microsoft.EntityFrameworkCore.Query.Internal;

namespace Bank;

[HarmonyPatch]
public class PatchClass(BasicMod mod, string settingsName = "Settings.json") : BasicPatch<Settings>(mod, settingsName)
{
    public override async Task OnWorldOpen()
    {
        Settings = SettingsContainer.Settings;

        if (Settings.VendorsUseBank)
            ModC.Harmony.PatchCategory(nameof(Debit));

        if (Settings.DirectDeposit)
            ModC.Harmony.PatchCategory(nameof(DirectDeposit));
    }

    public override void Stop()
    {
        base.Stop();

        if (Settings.VendorsUseBank)
            ModC.Harmony.UnpatchCategory(nameof(Debit));

        if (Settings.DirectDeposit)
            ModC.Harmony.UnpatchCategory(nameof(DirectDeposit));
}

    static string Currencies => string.Join(", ", Settings.Currencies.Select(x => x.Name));
    static string Commands => string.Join(", ", Enum.GetNames<Transaction>());

    [CommandHandler("bank", AccessLevel.Player, CommandHandlerFlag.RequiresWorld)]
    public static void HandleBank(Session session, params string[] parameters)
    {
        var player = session.Player;

        //Try to parse a valid command
        if (!parameters.TryParseCommand(out var verb, out var name, out var amount, out var wildcardAmount, out var recipient))
        {
            //player.SendMessage($"Usage: <command> [name|id] [amount=1]]\nAvailable commands: {Commands}");
            player.SendMessage($"+---------------BANK--------------V1.01----+\n");
            player.SendMessage($"Available commands: {Commands}\n");
            player.SendMessage($"/Bank List --- List your Bank contents.\n");
            player.SendMessage($"/Bank <Give/Take> [name|id] [amount=1]] --- Give or Take Items to/from Bank\n");
            player.SendMessage($"/Bank Send [recipient] [name|id] [amount=1] --- Send Items to a recipient.\n");
            return;
        }

        if (verb == Transaction.List)
        {
            HandleList(player);
            return;
        }

        //Parse information needed for other commands
        if (string.IsNullOrWhiteSpace(name))
        {
            player.SendMessage($"Specify the name or WCID of the item to transact with.");
            return;
        }

        //Try to parse weenie
        var query = int.TryParse(name, out var wcid) ?
            Settings.Items.Where(x => x.Id == wcid) :
            Settings.Items.Where(x => x.Name.StartsWith(name, StringComparison.OrdinalIgnoreCase));

        var item = query.FirstOrDefault();
        if (item is null)
        {
            player.SendMessage($"Unable to find matching item: {name}");
            return;
        }

        //Take the cap if it's smaller
        if (wildcardAmount || Settings.ExcessSetToMax)
        {
            var held = verb == Transaction.Give ? player.GetNumInventoryItemsOfWCID(item.Id) : (int)player.GetBanked(item.Prop);
            amount = Math.Min(amount, held);
        }

        switch (verb)
        {   case Transaction.Take:
                HandleWithdraw(player, item, amount);
                break;
            case Transaction.Give:
                HandleDeposit(player, item, amount);
                break;
            case Transaction.Send:
            {
                //player.SendMessage($"Send not implimented yet.\n");
                //player.SendMessage($"/Send recipient={recipient} name={name} amount={amount}.\n");
                TryHandleSend(player, recipient, item, amount);
                break;
            }

        }
    }

    //Handle other commands
    public static void HandleList(Player player)
    {
        player.SendMessage($"\nBankable Items:\n");
        foreach (var item in Settings.Items)
        {
            player.SendMessage($"{item.Name}, (WCID={item.Id})\n");
        }

        var sb = new StringBuilder("\nBanked items:");
        foreach (var item in Settings.Items)
        {
            //Skip missing?
            var banked = player.GetBanked(item.Prop);
            var held = player.GetNumInventoryItemsOfWCID(item.Id);

            if (Settings.SkipMissingBankedItems && banked <= 0 && held <= 0)
                continue;

            sb.Append($"\n{banked:0} {item.Name} banked, {held:0} held");
            //sb.Append($"\n{item.Name} (WCID={item.Id}):\n  {player.GetBanked(item.Prop)} banked, {player.GetNumInventoryItemsOfWCID(item.Id)} held");
            //sb.Append($"\n{item.Name} (WCID={item.Id}):\n  {player.GetBanked(item.Prop):0.00} banked, {player.GetNumInventoryItemsOfWCID(item.Id):0.00} held");
        }

        player.SendMessage($"{sb}");
    }

    [CommandHandler("lum", AccessLevel.Player, CommandHandlerFlag.RequiresWorld)]
    public static void HandleLum(Session session, params string[] parameters)
    {
        var player = session.Player;
        if (player is null) return;

        //Try to parse a valid command
        if (parameters.Length == 0 || !Enum.TryParse<Transaction>(parameters[0], true, out var verb))
        {
            //player.SendMessage($"/Lum Usage: <command>: {Commands}");
            player.SendMessage($"/Lum Usage: <command>: \nAvailable commands: List, Give, Take");
            return;
        }


        switch (verb)
        {
            case Transaction.List:
                player.SendMessage($"You have {player.GetBanked(Settings.LuminanceProperty):N0} luminance.");
                break;
            case Transaction.Give:
                var available = player.AvailableLuminance ?? 0;

                if (player.SpendLuminance(available))
                {
                    player.IncBanked(Settings.LuminanceProperty, (int)available);
                    player.SendMessage($"Stored {available} luminance.  You now have {player.GetBanked(Settings.LuminanceProperty):N0}.");
                    return;
                }
                break;
            case Transaction.Take:
                var stored = player.GetBanked(Settings.LuminanceProperty);
                var max = player.MaximumLuminance ?? 0;
                var missing = max - (player.AvailableLuminance ?? 0);
                var withdraw = (int)Math.Min(missing, stored);

                player.GrantLuminance(withdraw, XpType.Admin, ShareType.None);
                player.IncBanked(Settings.LuminanceProperty, -withdraw);
                player.SendMessage($"You've withdrawn {withdraw} luminance.  You now have {player.GetBanked(Settings.LuminanceProperty):N0}.");
                break;
            default:
                //player.SendMessage($"{verb} not implimented for luminance.\n");
                player.SendMessage($"/Lum Usage: <command>: \nAvailable commands: List, Give, Take");
                break;
        }
    }

    [CommandHandler("cash", AccessLevel.Player, CommandHandlerFlag.RequiresWorld)]
    public static void HandleCash(Session session, params string[] parameters)
    {
        var player = session.Player;
        if (player is null) return;


        if (!parameters.TryParseCommand(out var verb, out var name, out var amount, out var wildcardAmount, out var recipient))
        {
            //player.SendMessage($"/Cash Usage: <command> [name|id] [amount=1]]\nAvailable commands: {Commands}");
            player.SendMessage($"/Cash Usage: <command> [name|id] [amount=1]\nAvailable commands: List, Give, Take");
            return;
        }

        switch (verb)
        {
            case Transaction.List:
                player.SendMessage($"You have {player.GetBanked(Settings.CashProperty):N0} Pyreals.\nCurrencies: {Currencies}");
                return;
            //Deposit everything
            case Transaction.Give:
                //Get coins and tradenotes
                var cashItems = player.AllItems().Where(x => x.WeenieClassId == Player.coinStackWcid || x.WeenieClassName.StartsWith("tradenote"));
                long total = 0;
                foreach (var item in cashItems)
                    total += item.Value ?? 0;
                var itemCount = cashItems.Count();

                foreach (var item in cashItems)
                {
                    //Remove and if it fails don't count the value of the item
                    if (!player.TryRemoveFromInventoryWithNetworking(item.Guid, out var consumed, Player.RemoveFromInventoryAction.ConsumeItem))
                    {
                        //Log?
                        total -= consumed.Value ?? 0;
                        itemCount--;
                    }
                }

                player.IncCash(total);
                player.SendMessage($"Deposited {itemCount} currency items for {total:N0}.  You have {player.GetBanked(Settings.CashProperty):N0}");
                return;

            case Transaction.Take:
                if (string.IsNullOrWhiteSpace(name))
                {
                    player.SendMessage($"Specify currency.");
                    return;
                }

                //Parse currency
                var currency = Settings.Currencies.Where(x => x.Name.Equals(name, StringComparison.OrdinalIgnoreCase)).FirstOrDefault();
                if (currency is null || name == "")
                {
                    player.SendMessage($"Unable to find currency: {name}");
                    return;
                }

                //Withdraw amount
                long cost = (long)amount * currency.Value;

                //Check for overflow?

                // was long
                double stored = player.GetBanked(Settings.CashProperty);
                if (stored < cost)
                {
                    //Todo: decide on withdrawing cap?
                    //if(wildcardAmount || Settings.ExcessSetToMax)

                    player.SendMessage($"Insufficient funds: {cost} < {stored}");
                    return;
                }

                if (player.TryCreateItems($"{currency.Id} {amount}"))
                {
                    player.IncCash(-cost);
                    player.SendMessage($"Withdrew {amount} {currency.Name} for {cost:N0}.  You have {player.GetBanked(Settings.CashProperty):N0} remaining.");
                }
                else
                    player.SendMessage($"Failed to withdraw {amount} {currency.Name} for {cost:N0}.  You have {player.GetBanked(Settings.CashProperty):N0} remaining.");

                //player.UpdateCoinValue();
                return;
            default:
                player.SendMessage($"{verb} not implimented for Cash.\n");
                break;
        }
    }


    //Take items from the vault
    public static void HandleWithdraw(Player player, BankItem item, int amount)
    {
        var banked = player.GetBanked(item.Prop);

        if (banked < amount | banked == 0)
        {
            player.SendMessage($"Unable to withdraw {item.Name}.  You have {banked} {item.Name}");
            return;
        }

        //See if you can create items using the /ci approach
        if (player.TryCreateItems($"{item.Id} {amount}"))
        {
            player.IncBanked(item.Prop, -amount);
            player.SendMessage($"Withdrew {amount} {item.Name}. {player.GetBanked(item.Prop)} banked, {player.GetNumInventoryItemsOfWCID(item.Id)} held");
            
        }
    }

    public static void HandleDeposit(Player player, BankItem item, int amount)
    {
        if (player.TryTakeItems(item.Id, amount))
        {
            player.IncBanked(item.Prop, amount);
            player.SendMessage($"Deposited {amount:N0} {item.Name}. {player.GetBanked(item.Prop)} banked, {player.GetNumInventoryItemsOfWCID(item.Id):N0} held");
            return;
        }

        player.SendMessage($"Unable to deposit {item.Name}.  You have {player.GetNumInventoryItemsOfWCID(item.Id):N0} {item.Name}");
    }    
    
    /// <summary>
    /// Attempts to send a recipient some amount of a banked item
    /// </summary>
    public static bool TryHandleSend(Player player, string recipient, BankItem item, int amount)
    {
        var banked = player.GetBanked(item.Prop);
        player.SendMessage($"/bank send {recipient} {item.Name} {amount} | banked={banked} {item.Name}.\n");
        if (banked < amount || banked == 0)
        {
            player.SendMessage($"Unable to withdraw {amount}.  You have {banked} {item.Name}");
            return false;
        }
        
        //var alts = PlayerManager.GetAllOffline().Where(x => player.Account.AccountId == x.Account.AccountId);
        var alts = PlayerManager.GetAllOffline().Where(x => player.Name != x.Name);
        if (alts is null)
        {
            //player.SendMessage($"No other players found on this account.");
            player.SendMessage($"No other players found Offline.");
            return false;
        }

        //var r = alts.Where(x => x.Name.Contains(recipient, StringComparison.OrdinalIgnoreCase)).FirstOrDefault();
        var r = alts.Where(x => x.Name.Equals(recipient, StringComparison.OrdinalIgnoreCase)).FirstOrDefault();
        if (r is null)
        {
            //player.SendMessage($"Recipient {recipient} was not found Offline: \nalts:\n{string.Join("\n", alts.Select(x => $"{x.Name} - {x.Level}"))}");
            player.SendMessage($"Recipient {recipient} was not found Offline: \n");
            return false;
        }

        player.IncBanked(item.Prop, -amount);

        r.IncBanked(item.Prop, amount);

        player.SendMessage($"Sent {amount:N0} {item.Name}(s) from {player.Name} to {r.Name}.\n{banked - amount} remaining.");
        return true;
    }

}

public static class BankExtensions
{
    // was long
    public static double GetCash(this Player player) => player.GetBanked(PatchClass.Settings.CashProperty);
    public static void IncCash(this Player player, long amount)
    {
        player.IncBanked(PatchClass.Settings.CashProperty, amount);
        player.UpdateCoinValue();
    }

    // was long and PropertyInt64 
    public static double GetBanked(this Player player, int prop) =>
        player.GetProperty((PropertyFloat)prop) ?? 0;
    public static void IncBanked(this Player player, int prop, long amount) =>
        player.SetProperty((PropertyFloat)prop, player.GetBanked(prop) + amount);
       
    public static double GetBanked(this OfflinePlayer player, int prop) =>
        player.GetProperty((PropertyFloat)prop) ?? 0;
    public static void IncBanked(this OfflinePlayer recipient, int prop, double amount) =>
        recipient.SetProperty((PropertyFloat)prop, recipient.GetBanked(prop) + amount);
     
    //Parsing
    static readonly string[] USAGES = new string[] {
        $@"(?<verb>{Transaction.List})$",
        //First check amount first cause I suck with regex
        $@"(?<verb>{Transaction.Give}|{Transaction.Take}) (?<name>.+)\s+(?<amount>(\*|\d+))$",
        $@"(?<verb>{Transaction.Give}|{Transaction.Take}) (?<name>.+)$",
        // /cash doesn't have named item
        $@"(?<verb>{Transaction.Give})$",
        $@"(?<verb>{Transaction.Send}) (?<recipient>.+) (?<name>.+)\s+(?<amount>(\*|\d+))$",
        $@"(?<verb>{Transaction.Send}) (?<recipient>.+) (?<name>.+)$",

    };
    //Join usages in a regex pattern
    static string Pattern => string.Join("|", USAGES.Select(x => $"({x})"));
    static Regex CommandRegex = new(Pattern, RegexOptions.IgnoreCase | RegexOptions.Compiled);

    public static bool TryParseCommand(this string[] parameters, out Transaction verb, out string name, out int amount, out bool wildcardAmount, out string recipient)
    //V1.01 add recipient
    {
        //Set defaults
        amount = 1;
        verb = 0;
        name = null;
        recipient = null;
        wildcardAmount = false;

        //Debugger.Break();
        //Check for valid command
        var match = CommandRegex.Match(string.Join(" ", parameters));
        if (!match.Success)
            return false;

        //Parse verb
        if (!Enum.TryParse(match.Groups["verb"].Value, true, out verb))
            return false;

        //Set name
        name = match.Groups["name"].Value;
        
        //V1.01 Set recipient if available
        recipient = match.Groups["recipient"].Value;

        //Parse amount if available
        if (int.TryParse(match.Groups["amount"].Value, out var parsedAmount))
            amount = parsedAmount;
        else if (match.Groups["amount"].Value == "*")
        {
            amount = int.MaxValue;
            wildcardAmount = true;
        }

        return true;
    }

    //Support for spaces in names
    public static string ParseName(this string[] param, int skip = 1, int atEnd = 0) => param.Length - skip - atEnd > 0 ?
        string.Join(" ", param.Skip(skip).Take(param.Length - atEnd - skip)) : "";

    //Parse quantity from last parameter supporting wildcards
    public static bool TryParseAmount(this string[] param, out int amount, int max = int.MaxValue)
    {
        var last = param.LastOrDefault() ?? "";

        //Default to 1
        amount = 1;

        bool success = true;

        //Check for wildcards/other handling
        if (last == "*")
            amount = int.MaxValue;
        else if (int.TryParse(last, out var parsedAmount))
            amount = parsedAmount;
        //Amount was not parsed
        else
            success = false;

        //Wildcards will always use the max value, parsed ints will use the setting
        if (PatchClass.Settings.ExcessSetToMax || last == "*")
            amount = Math.Min(max, amount);

        return success;
    }
}

public enum Transaction
{
    List,
    Give,
    Take,
    Send,
}
