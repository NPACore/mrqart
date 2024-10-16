sim/src: EPI_first10.zip sim/
	unzip -d $@ EPI_first10.zip

EPI_first10.zip:
	find -type f /Volumes/Hera/Raw/MRprojects/Habit/2024.10.04-14.30.36/12049_20241004/HabitTask_704x752.14/ |head| zip -@ --junk-paths EPI_first10.zip

config/smb.conf: /etc/samba/smb.conf
	cp $< $@
clear-data: config/smb.conf
	find $(shell grep -Po '(?<=path = )/.*' config/smb.conf) -mindepth 1 -mtime +2 -exec echo rm -r {} \+
	
%/:
	mkdir -p $@
