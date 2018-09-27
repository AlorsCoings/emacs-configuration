# Setup hard drive
echo "Add a line to /etc/fstab to mount hdd at startup"
echo "
# Mount hdd to /media/hdd on startup
UUID=1b1d3e20-407c-42c4-bc7f-db5b021d13c3 /media/hdd ext4 defaults 0 1
" | sudo tee --append /etc/fstab

# Use hard drive directory
echo "Remove original ~/Videos directory"
rmdir ~/Videos
echo "Link hdd Videos directory to home Videos directory"
ln -s /media/hdd/Videos ~/Videos

echo "Remove original ~/Music directory"
rmdir ~/Music
echo "Link hdd Music directory to home Music directory"
ln -s /media/hdd/Music ~/Music

echo "Remove original ~/Pictures directory"
rmdir ~/Pictures
echo "Link hdd Pictures directory to home Pictures directory"
ln -s /media/hdd/Pictures ~/Pictures

echo "Remove original ~/Downloads directory"
rmdir ~/Downloads
echo "Link hdd Downloads directory to home Downloads directory"
ln -s /media/hdd/Downloads ~/Downloads

# Crontab save home
echo "# Edit this file to introduce tasks to be run by cron.
#
# Each task to run has to be defined through a single line
# indicating with different fields when the task will be run
# and what command to run for the task
#
# To define the time you can provide concrete values for
# minute (m), hour (h), day of month (dom), month (mon),
# and day of week (dow) or use '*' in these fields (for 'any').#
# Notice that tasks will be started based on the cron's system
# daemon's notion of time and timezones.
#
# Output of the crontab jobs (including errors) is sent through
# email to the user the crontab file belongs to (unless redirected).
#
# For example, you can run a backup of all your user accounts
# at 5 a.m every week with:
# 0 5 * * 1 tar -zcf /var/backups/home.tgz /home/
#
# For more information see the manual pages of crontab(5) and cron(8)
#
# m h  dom mon dow   command
30 4 * * * /home/toad/scripts/saveHome.sh
" | crontab -



