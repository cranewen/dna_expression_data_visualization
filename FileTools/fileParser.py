import os
import re
import xlsxwriter
import csv
import path
from numpy.core.defchararray import upper

#File parser is a tool reads raw GraphPad file.
#Then creates the directories as GraphPad file names.
#It creates spreadsheets for each gene under the directory just created.
#It reads all the spreadsheets get conditions and genes (through) the file name.
#Extracting title(GraphPad file name) information into species, tissues, disease, date

class FileParser:
	def __init__(self, path):
		self.path = path

	directoryNames = []
	dir_files_dict = {}

	def getDirectoryNames(self):
		lenOfPath = len(self.path)
		for dirNames in os.walk(self.path):
			for dir in dirNames:
				lenOfDir = len(dir)
				if os.path.isdir(str(dir)) and dir != self.path:
					self.directoryNames.append(dir)
		# self.directoryNames.remove(self.path) # remove the top directory we just inputed
		# self.directoryNames.pop(0)
		return self.directoryNames



	# return a dictionary with {folderName(key):fileNames(value(array))
	def getFileNames(self):
		fileNames = []
		for d in self.directoryNames:
			fileName = ''
			for fileName in os.listdir(d):
				if not os.path.isdir(fileName):
					fileNames.append(fileName)
			self.dir_files_dict[self.filterDirectoryName(d)] = fileNames
			fileName = []
		return fileNames

	# filter single directory name
	def filterDirectoryName(self, dName):
		dirName = dName.split('\\')
		return dirName[-1]

	def filterDirectoryNames(self, dNames):
		filteredDirNames = []
# 		for d in dNames:
# 			filteredDirNames.append(re.findall(r'.*\\(.+?)',d))
		for d in dNames:
			temp = d.split('\\')
			filteredDirNames.append(temp[-1])
		return filteredDirNames

	#Get a gene name list from all the gene file names.
	def getGeneNames(self):
		fileNames = []
		geneNames = []
		for foldName in os.listdir(self.path):
			subPath = ''
			foldName = self.path + "/" + foldName
			if os.path.isdir(foldName):
				for fileName in os.listdir(foldName):
					fileNames.append(self.getGeneName(fileName))

		uniqeGeneNames = list(set(fileNames))

		return uniqeGeneNames

	# A gene name filter from csv file name.
	def getGeneName(self,s):
		subString = re.search(r"(s|Data 3-)(.+?)(.csv)", s).group(2)
		#or using
		#subString = re.search(r"(.+[3]-)(.+?)(.csv)", s).group(2)

		return subString

	# get conditions from all the files under a folder and removed duplicates
	def getConditions(self, path):
		directoryNames = self.getDirectoryNames()
		conditions = []

		for d in directoryNames:
			for filename in os.listdir(d):
				filePathName = d + "\\" + filename
				with open(filePathName, 'r') as f:
					data = csv.reader(f)
					row1 = next(data)
					for i in row1:
						conditions.append(i)
						conditions = list(set(conditions))
		print conditions
		return conditions

	#Get all the gene names and make dictionaries {gene_alias:gene_name} !!!
	# def getGeneNamesWithAlias(self, s):
	# 	alias_genename_dict = {}
	# 	for g in s:
	# 		tempG = g.upper()
	# 		tempG = re.sub(r'[-]','', tempG)
	# 		tempG = re.sub(r'[ ]','', tempG)
	# 		tempG = re.sub(r'\(.+?\)','', tempG)
	# 		if (tempG == 'CREB' or tempG == 'TGFB'):
	# 			tempG = tempG + '1'
	# 		alias_genename_dict[g] = tempG

	# 	return alias_genename_dict

	def writeToExcel(self, l, wbName, wsName):
		wb = xlsxwriter.Workbook(wbName)
		ws = wb.add_worksheet(wsName)
		for i in range(len(l)):
			ws.write(i, 0, l[i])
		wb.close()



#The class handles GraphPad files under the folder, and it creates folders as the
#same names of GraphPad file names.

class DirectoryCreator:
	def __init__(self, graphPadPath):
		self.graphPadPath = graphPadPath

	# Create the folders, also return the list of folder names.
	def createFolders(self, pathToWrite):
		directoryNames = []
		for fname in os.listdir(self.graphPadPath):
			name, ext = fname.split(".")
			directoryNames.append(name)

		for directoryName in directoryNames:
			os.mkdir(pathToWrite + directoryName, 0755)
		return directoryNames;

# x = FileParser("\data_path")
# # print x.getGeneNamesWithAlias(x.getGeneNames())
# x.getDirectoryNames()
# # x.getFileNames()
# print x.getFileNames()
