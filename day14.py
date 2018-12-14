def gotonext(idx, amt):
    return (idx + amt) % len(recipes)


recipes = [3, 7]

firstworker = 0
secondworker = 1
input = 920831
size = 1_000_000_00


while len(recipes) < size:
    ss = recipes[firstworker] + recipes[secondworker]
    recipes.extend(int(d) for d in str(ss))
    firstworker = gotonext(firstworker, recipes[firstworker] + 1)
    secondworker = gotonext(secondworker, recipes[secondworker] + 1)

print(''.join(str(i) for i in recipes[input:input + 10]))
a = ''.join(str(i) for i in recipes)
print(a.find(str(input)))
