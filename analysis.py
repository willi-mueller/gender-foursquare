import csv

def read_csv(filename):
    l = []
    with open(filename) as f:
        reader = csv.reader(f)
        for row in reader:
            l.append(row)
    # exclude header
    return l[1:]

def count2f(x):
    for c in x:
        c[4] = float(c[4])
    return x


def diff_pref_outliers(x, y, threshold=0.2):
    l = []
    for m, f in zip(y, x):
        diff = round(m[4] - f[4], 3)
        if abs(diff) > threshold:
            res = (diff, m[1], m[2], m[4], f[4])
            print ("diff m-f:%s, %s, %s, %s, %s"%res)
            l.append(res)
    return l


if __name__ == '__main__':
    x = read_csv("analysis/sweFeCollapsed.csv")
    y = read_csv("analysis/sweMaCollapsed.csv")

    count2f(x)
    count2f(y)
    diff_pref_outliers(x,y)