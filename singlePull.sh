for d in /Volumes/Hera/Raw/MRprojects/Habit/*/*/RewardedAntisaccade_704x75*; do 
    find "$d" -iname 'MR*' -exec sh -c '
        dicom_hdr "$1" | grep -E "Repetition Time"
    ' sh {} \; -quit
done
