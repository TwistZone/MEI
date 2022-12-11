
def remover(s1,s2,l,idx,idx2):

    s = ""
    for i in range(l):
       if i == idx-1:
          break
       if s1[i] == s2[i]:
         if i == idx2:
            break
         s = s + s2[i]
       else:
          break
    return s

def prep(file):
    with open(file,"a+") as f:
        f.seek(0)  
        lines = f.readlines()
        print(len(lines))
        f.truncate(0)
        for l in lines: 
            splt = l.split()  
            d = splt[0]
            m = splt[1]
            lg = len(m) if len(m) < len(d) else len(d)
            idx = m.rfind(".") if m.rfind(".") < d.rfind(".") else d.rfind(".")
            idx2 = m.rfind("e") if m.rfind("e") < d.rfind("e") else d.rfind("e")
            e = splt[2]
            s = remover(d,m,lg,idx,idx2)
            if int(splt[3]) < 2500:
                dix = d.find(".")
                mix = m.find(".")
                d = d[dix-1:len(d)]
                m = m[mix-1:len(m)]   
                e = e.replace(s,"",1)
                eix = e.find(".")
                e = e.replace ("-1","",1) if eix > e.find("-1") else e
                eix = e.find(".")
                e = e[eix-1:len(e)] 
            else:
                d = d.replace(s,"",1)
                m = m.replace(s,"",1)
                eix = e.find(".")
                e = e.replace ("-1","",1) if eix > e.find("-1") else e
                e = e.replace(s,"",1)  
            nl = d + " " + m + " " + e + " " + splt[3] + " " + splt[4] + " " + splt[5] + " " + splt[6] +"\n"
            f.write(nl)

if __name__ == "__main__":
    print("Starting")
    prep("processing.txt")
    print("Finished")
