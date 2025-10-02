for d in /Volumes/Hera/Raw/MRprojects/Habit/*/*/RewardedAntisaccade_704x75*; do 
    find "$d" -iname 'MR*' -exec sh -c '
        dicom_hdr "$1" | grep -E "Repetition Time|Echo Time|Acquisition Matrix|Pixel Spacing|Magnetic Field Strength|Flip Angle|Pixel Bandwidth|Sequence Name|Protocol Name|Image Comments|Phase Encoding Direction|Acquisition Time"
    ' _ {} \; -quit
done

