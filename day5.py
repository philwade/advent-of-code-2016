import md5

input = "wtnhxymk"

password = [" ", " ", " ", " ", " ", " ", " ", " "]

start = md5.new()
start.update(input)
count = 0

while " " in password:
    newHash = start.copy()
    newHash.update(str(count))
    hash = newHash.hexdigest()

    if hash[:5] == "00000":
        index = hash[5]
        val = hash[6]

        try:
            realVal = val
            realIndex = int(index)
            if password[realIndex] == " ":
                password[realIndex] = realVal
            print password

        except ValueError:
            pass
        except IndexError:
            pass

    count += 1

