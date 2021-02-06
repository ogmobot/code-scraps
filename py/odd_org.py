# Simulates "Into the Odd" organisations

import random

d = lambda n: random.randint(1, n)

# Organisations are
# {"id": <int>,
#  "wealth": <int>,
#  "enterprises": <list>,
#  "detachments": <list>}

G_ID_COUNT = 0

# Enterprises are <int>s, representing income:
# 4 -> 6 -> 8 -> 10 -> 12
MAX_ENTERPRISES = 5

# Detachments are
# {"hp": <int>,
#  "exp": <int>}
LEVEL_TITLES = {0:"Novice", 1:"Professional", 4:"Expert", 9:"Veteran", 13:"Master"}
MAX_DETACHMENTS = 5

def det_level(det):
    result = None
    for exp, title in LEVEL_TITLES.items():
        if det["exp"] >= exp and (result == None or result[0] < exp):
            result = (exp, title)
    return result[1]

new_enterprise = lambda: 4
new_detachment = lambda: {"hp":d(6), "exp":0}
def new_organisation():
    global G_ID_COUNT
    org = {"id": G_ID_COUNT, "age": 0, "wealth": d(6) + d(6), "enterprises": [], "detachments": []}
    G_ID_COUNT += 1
    if d(2) == 1:
        org["enterprises"].append(new_enterprise())
    else:
        org["detachments"].append(new_detachment())
    return org

def maybe_level_up(det):
    # Checks whether a detachment has reached exp necessary to gain a level
    # Doesn't check whether the company has taken on an apprentice
    if det["exp"] in LEVEL_TITLES.keys():
        # Gain a level
        det["hp"] += d(6)
    return

def step_org(org, verbose):
    if verbose: print("Stepping org id:{0:02} wealth:{1:3}".format(org["id"], org["wealth"]))
    org["age"] += 1
    # Pay detachments
    for i in range(len(org["detachments"])):
        cost = d(6)
        if cost > org["wealth"]:
            # Flag detachment for deletion
            if verbose: print("  A detachment was disbanded: {}".format(org["detachments"][i]))
            org["detachments"][i] = None
        else:
            org["wealth"] -= cost
    # Each detachment has a 75% chance to go on a mission
    for i in range(len(org["detachments"])):
        if d(4) > 1:
            difficulty = d(6)
            det = org["detachments"][i]
            if det: det_score = d(det["hp"])
            if det and det_score >= difficulty:
                # mission successful
                det["exp"] += 1
                maybe_level_up(det)
                loot = det_score + 2*difficulty + d(6)
                org["wealth"] += loot
                if verbose: print("  A detachment {}completed a mission: {}".format("leveled up and " if det["exp"] in LEVEL_TITLES.keys() else "", det))
                if verbose: print("    (profit of {})".format(loot))
            else:
                # detachment destroyed
                if det and verbose: print("  A detachment was destroyed: {}".format(det))
                org["detachments"][i] = None
    # Remove detachments flagged for deletion
    org["detachments"] = [det for det in org["detachments"] if det != None]
    # Each enterprise attempts to profit
    for i in range(len(org["enterprises"])):
        ent = org["enterprises"][i]
        profit = d(ent)
        threat = d(ent)
        # Detachments help to deal with threats;
        # Chance of failure is 1 / (n + 1), where n is the number of detachments
        #   0 => 100% failure
        #   1 => 50% failure
        #   2 => 33% failure
        #   3 => 25% failure
        #   4 => 20% failure, etc.
        if d(len(org["detachments"]) + 1) == 1:
            # threat came to pass
            profit -= threat
        else:
            if verbose: print("  A threat to an enterprise was overcome")
        # If enterprise makes a loss, the organisation will let it fail with a x-in-y chance, where x is its size and y is the org's wealth
        if profit >= 0 or ((profit + org["wealth"] > 0) and d(org["wealth"]) > ent):
            org["wealth"] += profit
            if profit > 0 and ent < 12:
                org["enterprises"][i] += 2
            if verbose: print("  An enterprise (d{}) {}".format(ent, "broke even" if profit == 0 else (f"made a profit of {profit}" if profit > 0 else f"made a loss of {-profit}")))
        else:
            if verbose: print(f"  An enterprise (d{ent}) went bankrupt")
            org["enterprises"][i] = None
    # Remove enterprises flagged for deletion
    org["enterprises"] = [ent for ent in org["enterprises"] if ent != None]
    # 1-in-(n+4) chance to add a new enterprise or detachment if possible
    if org["wealth"] > 10 and d(len(org["enterprises"]) + 4) == 1 and len(org["enterprises"]) < MAX_ENTERPRISES:
        org["wealth"] -= 10
        org["enterprises"].append(new_enterprise())
        if verbose: print("  Started a new enterprise.")
    # Detachments not as essential
    if org["wealth"] > 10 and d(len(org["detachments"]) + 4) == 1 and len(org["detachments"]) < MAX_DETACHMENTS:
        org["wealth"] -= 10
        org["detachments"].append(new_detachment())
        if verbose: print("  Recruited a new detachment.")
    return

def main():
    verbose = True
    num_orgs = 10
    num_generations = 20
    orgs = [new_organisation() for i in range(num_orgs)]
    for t in range(num_generations):
        if verbose:
            print(f"t = {t}")
        for i in range(len(orgs)):
            org = orgs[i]
            step_org(org, verbose)
            if len(org["enterprises"]) + len(org["detachments"]) == 0:
                orgs[i] = None
                if verbose: print("  The organisation was dissolved.")
        orgs = [org for org in orgs if org != None]
        while len(orgs) < num_orgs:
            orgs.append(new_organisation())
    for org in orgs:
        print("Organisation id:{:3} (age:{:2})".format(org["id"], org["age"]))
        print("  Wealth: {}".format(org["wealth"]))
        if org["detachments"]:
            print("  Detachments: " + ", ".join(["{} (HP:{:2})".format(det_level(det), det["hp"]) for det in org["detachments"]]))
        if org["enterprises"]:
            print("  Enterprises: " + ", ".join(["d{}".format(ent) for ent in org["enterprises"]]))
    return

if __name__=="__main__":
    main()
