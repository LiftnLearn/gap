for i in [1,3..5] do
    Print("hello");
od;

if(true) then
  Print("hello1");
elif(true) then
  Print("hello2");
else
  Print("else");
fi;

i:=0;
while (true or false) do
  i := i + 1;
  Print((1,2)(4,3));
  Print([1,2,[1]]);
od;

l := [3];
l[1] := 2;

r := rec(a:=1);
