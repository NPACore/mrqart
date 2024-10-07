##!/usr/bin/env bashv

# Help                                                     #
help()
{
   # Display Help
   echo "runscan script to call gyrus2 ScanBOLDDWI."
   echo
   echo "Syntax: ./runscan.sh [p|S|E|s|e|h]"
   echo "options:"
   echo "p     Project"
   echo "S     StartYear"
   echo "E     EndYear"
   echo "s     StartDate"
   echo "e     EndDate"
   echo "h     help."
   echo "./runscan.sh -p WPC-7366 -S 2018 -E 2019 -s 0101 -e 0202" 
   echo
}

# Input arguments
while getopts p:S:E:s:e:h flag
do
    case "${flag}" in
        p) Project=${OPTARG};;
        S) StartYear=${OPTARG};;
        E) EndYear=${OPTARG};;
        s) StartDate=${OPTARG};;
        e) EndDate=${OPTARG};;
        h) help
            exit;;
        \?) # Invalid option
         echo "Error: Invalid option"
         exit;;
    esac
done

# Server
server=moonc@10.48.88.13

# Echo
echo ${Project} from ${StartYear}${StartDate} to ${EndYear}${EndDate}

# Run
##ssh -i ~/.ssh/macst2gyrus2 ${server} "cd ~/sshremote/ScanBOLDPhaseEnc; chmod 775 *; rm *.xlsx; matlab -nodisplay -r \"clear all; close all; runscan('${Project}', ${StartYear}, ${EndYear}, '${StartDate}', '${EndDate}'); quit;\" "
#ssh ${server} "cd ~/sshremote/ScanBOLDPhaseEnc; chmod 775 *; rm *.xlsx; matlab -nodisplay -r \"clear all; close all; runscan('${Project}', ${StartYear}, ${EndYear}, '${StartDate}', '${EndDate}'); quit;\" "
ssh ${server} "cd ~/sshremote/ScanBOLDPhaseEnc; chmod 775 *; rm *.xlsx; matlab -batch \"clear all; close all; runscan('${Project}', ${StartYear}, ${EndYear}, '${StartDate}', '${EndDate}'); quit; quit;\" "

# Copy file
#scp -i ~/.ssh/macst2gyrus2 ${server}:~/sshremote/ScanBOLDPhaseEnc/*.xlsx .
scp ${server}:~/sshremote/ScanBOLDPhaseEnc/*.xlsx .
