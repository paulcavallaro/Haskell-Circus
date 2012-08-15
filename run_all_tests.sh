echo "Testing pylex..."

for input in *.py
do
    echo 
    echo "---------------------------------------------------------------"
    echo "Testing $input..."
    echo "---------------------------------------------------------------"
    cat $input | ./pylex
done
