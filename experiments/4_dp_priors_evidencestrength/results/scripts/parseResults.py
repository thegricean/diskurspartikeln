import json,re,os

subject = 0
outfile = open("../data/results.csv","w")
		
for f in os.listdir("../data/raw"):
	print "processing...."+f
	datafile = open("../data/raw/"+f)
	data = [l.rstrip() for l in datafile.readlines()]
	datafile.close()
		
	datadict = eval(data[0])
	
	headers = ["subject"]+datadict["trials"][0].keys() + datadict["system"].keys() + datadict["subject_information"].keys() + ["time_in_minutes"]
	
#	print datadict["trials"]
#	print datadict["system"]
#	print datadict["subject_information"]
#	print datadict["time_in_minutes"]
#	print headers
	if subject == 0:
		outfile.write("\t".join(headers)+"\n")
	
	system = []
	subject_info = []
	for s in datadict["system"].keys():
		system.append(str(datadict["system"][s]))
		
	for s in datadict["subject_information"].keys():
		subject_info.append(str(datadict["subject_information"][s]))	
	
#	print system
#	print subject_info
		
	for t in datadict["trials"]:
		trial = []
		for k in t.keys():
			trial.append(str(t[k]))
		outfile.write("\t".join([str(subject)]+trial+system+subject_info+[str(datadict["time_in_minutes"])])+"\n")

	subject = subject + 1		

outfile.close()	