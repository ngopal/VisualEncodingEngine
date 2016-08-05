
# Copying Admin Page to Participant View
#cp www/index.html www/participantview.html
# Need to edit participantview.html file after copying
# Edit #cy stylesheet
# Edit to add hideExtraneous() to ready
#

# Edge Pages
for i in `seq 1 15`
do
echo $i
cp www/participantview.html www/edges/page$i.html
done

# Node Pages
for i in `seq 1 28`
do
echo $i
cp www/participantview.html www/nodes/page$i.html
done
