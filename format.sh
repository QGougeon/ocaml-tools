for file in $(ls *.ml*)
do
	sed -i "s/\t/  /g" $file
	sed -i "s/ $//g" $file
done
echo "issues with \\t"
grep -R $'\t' *.ml*
echo "tailling space issues"
grep -R " $" *.ml*
