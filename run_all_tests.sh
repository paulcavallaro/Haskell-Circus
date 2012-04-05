echo "Testing Tokens..."

for input in simple_def.py string_literals.py byte_literals.py integer_literals.py float_literals.py imaginary_literals.py indent.py
do
    echo 
    echo "---------------------------------------------------------------"
    echo "Testing $input..."
    echo "---------------------------------------------------------------"
    foo=$(cat $input | ./Tokens)
    echo $foo
done
