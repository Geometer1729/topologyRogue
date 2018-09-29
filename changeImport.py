import glob

print("Module currently there")
current = input(">>> ")
print("Module to replace with")
new = input(">>> ")

def editfile(fname):
    global current, new
    newcode = []
    with open(fname) as f:
        for l in f.readlines():
            if "import" in l:
                newcode.append(l.replace(current,new))
            else:
                newcode.append(l)
    print(newcode)
    with open(fname,'w') as f:
        for line in newcode:
            f.write(line)

for fname in glob.iglob("src/**/*.hs",recursive=True):
    print(fname)
    editfile(fname)
