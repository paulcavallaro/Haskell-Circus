echo "Testing Tokens..."

for input in simple_def.py string_literals.py byte_literals.py integer_literals.py float_literals.py imaginary_literals.py
do
    echo "\n---------------------------------------------------------------"
    echo "Testing $input..."
    echo "---------------------------------------------------------------"
    foo=$(cat $input | ./Tokens)
    echo $foo
done