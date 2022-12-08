add_math_scripts_to_path() {
    # cp the scripts to a path in the $PATH
    for file in ~/Projects/Math_Scripts/Scripts/*; do
	case $file in ~/Projects/Math_Scripts/Scripts/__pycache__)
	    continue
	esac
	cp $file ~/bin/Math_Scripts/
    done

    # make them executable and change name to remove extension
    for file in ~/bin/Math_Scripts/*; do
	case $file in ~/bin/Math_Scripts/Printing.py)
	    cp $file ~/bin/
	    continue
	esac
	 perl -pi -e 'print "#! /usr/bin/env python3\n" if $. == 1' $file
	 chmod +x $file
	 new_name=$(basename -s .py $file)
	 mv $file ~/bin/Math_Scripts/$new_name
	 # move the files back to the dir that is on the path
	 cp ~/bin/Math_Scripts/$new_name ~/bin/
    done
}
