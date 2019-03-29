OFFSET = 213
# Sub this from real date to get Nev. date
# assuming Aug 1 == ILE 0

seasons = [
    # 61 days each
    "ile", # Aug + Sep
    "ahn", # Oct + Nov
    "nik", # Dec + Jan
    "syv", # Feb + Mar
    "wek", # Apr + May
    "are"  # Jun + Jul
]
months = ['jan','feb','mar','apr','may','jun','jul','aug','sep','oct','nov','dec']
days_in_month = [31, 29, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31]
month_key = dict(zip(months,days_in_month))

intro_message = """How to use:
?Jan 1
 => Nik 31
?December 31
 => Nik 30
?Ile 0
 => Aug 1
?Arevael 60
 => Jul 31
?

Note that "Ilelest 0" indicates the holiday of that season.
"""

def days_real(month, day):
    # Days since 1st Jan
    month = month[:3].lower()
    assert month in months
    total = 0
    for i, m in enumerate(months):
        if m == month:
            return total + day
        else:
            total += month_key.get(m,0)
    return

def days_nev(season, day):
    # Days since 0th ILE
    # Holidays are the 0th of the month
    season = season[:3].lower()
    assert season in seasons
    total = 1
    for i, s in enumerate(seasons):
        if s == season:
            return total + day
        else:
            total += 61
    return

def days2date_real(days):
    month = 0
    while days > 0:
        if days > days_in_month[month]:
            days -= days_in_month[month]
            month += 1
        else:
            return (months[month], days)

def days2date_nev(days):
    season = 0
    while days > 61:
        season += 1
        days -= 61
    return (seasons[season], days-1)

def convert2nev(month, day):
    days = days_real(month, day)
    days -= OFFSET
    days = days % 366
    return days2date_nev(days)

def convert2real(season, day):
    days = days_nev(season, day)
    days += OFFSET
    days = days % 366
    return days2date_real(days)

def main():
    print(intro_message)
    words = input("?")
    while words:
        parts = words.split()
        name = parts[0][:3].lower()
        day = int(parts[1])
        if name in months:
            result = convert2nev(name, day)
        elif name in seasons:
            result = convert2real(name, day)
        else:
            result = ("Month not found.",)
        
        print(" => {}".format(" ".join([str(s).title() for s in result])))

        words = input("?")

if __name__=="__main__":
    main()
