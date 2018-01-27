var1 <-25
var2 <-50
myfunc1 <-function()
{
  print("Inside function myfunc1");
  print("Environment is: ");
  print(environment());
  print("var2 is:");
  print(var2);
  var2 <-55;
  print("Local (but not global) var2 is modified to:");
  print(var2);
}
myfunc2 <-function()
{
  print("Inside function myfunc2");
  print("Environment is: ");
  print(environment());
  var2 <<-80;
  var3 <<-678;
  var4 <-444;
  print("Variables inside the myfunc2 environment:");
  ls();
}
print("In the global environment");
environment()
myfunc1()
print("In the global environment");
print("Global var2:")
print(var2)
myfunc2()
print("In the global environment");
print("Global var2 has been modified within myfunc2 to:");
print(var2)
print("Global var3 has been created within myfunc2:");
print(var3)
print("Variables inside the global environment:");
ls();
print("Local var4 created within myfunc2 is NOT seen in the global environment
scope:");
print(var4)


