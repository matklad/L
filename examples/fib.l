read(n);
f1 := 1;
f2 := 1;
i := 0;
while (i < n) {
 i := i + 1;
 f2 := f2 + f1;
 f1 := f2 - f1
};
write(f1)
