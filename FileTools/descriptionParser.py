import os
import re
import csv
import os.path
from numpy.core.defchararray import upper
from datetime import datetime

# This is a parser that reads all the folders' names.
# It returns description titles and all possible information we can get from the folder name.
# Such as Tissue, Species, Compound, Date...
class DescriptionParser:
	def __init__(self, path):
		self.path = path

		self.description_dict = {}
		# Get descriptions from all the folder names(Description titles).
		self.directoryNames = []
		lenOfPath = len(self.path)
		for dirNames in os.walk(self.path):
			for dir in dirNames:
				lenOfDir = len(dir)
				if os.path.isdir(str(dir)) and dir != self.path:
					self.directoryNames.append(self.filterDirectoryName(dir))

		self.compound = [1701, 1973]
		self.tissues = ['KIDNEY', 'HEART', 'BRAIN', 'LIVER', 'LUNG', 'RIGHT KIDNEY', 'LEFT KIDNEY', 'EYE']
		self.species = ['RAT', 'MICE', 'MOUSE']

	# def getDirectoryNames(self):
	# 	lenOfPath = len(self.path)
	# 	for dirNames in os.walk(self.path):
	# 		for dir in dirNames:
	# 			lenOfDir = len(dir)
	# 			if os.path.isdir(str(dir)) and dir != self.path:
	# 				self.directoryNames.append(self.filterDirectoryName(dir))
	# 	# return self.directoryNames
	# self.getDirectoryNames()

	def filterDirectoryName(self, dName):
		dirName = dName.split('/')
		return dirName[-1]
		
	def filterDate(self, d):
		filteredDate = re.sub('-corrected', '', d, flags=re.IGNORECASE)
		filteredDate = re.sub(r'(.+?)(-.+?-)(.+?)', r'\1\g<2>20\3', filteredDate)
		return filteredDate

	# Return a dictionary of {description: date}	
	def getDate(self):
		description_date_dict = {}
		descriptions = self.directoryNames
		for d in descriptions:
			description_date_dict[d] = self.filterDate(d.split(' ')[-1])
		return description_date_dict

	def getDescritpionDict(self):
		for title in self.directoryNames:
			titleSplitter = title.split(' ')
			self.description_dict[title] = {}
			for comp in titleSplitter:
				if (comp in self.compound):
					self.description_dict[title]['compound'] = comp
			for i in range(len(titleSplitter)):
				tiss = titleSplitter[i]
				if (tiss.upper() == 'LEFT' or tiss.upper() == 'RIGHT'):
					if (titleSplitter[i + 1].upper() in self.tissues):
						temp = tiss.upper() + ' ' + titleSplitter[i + 1].upper()
						self.description_dict[title]['tissue'] = temp
						break
				elif (tiss.upper() in self.tissues):
					self.description_dict[title]['tissue'] = tiss.upper()
			for spec in titleSplitter:
				if (spec.upper() in self.species):
					self.description_dict[title]['species'] = spec.upper()
			description_date = datetime.strptime(self.filterDate(titleSplitter[-1]), '%m-%d-%Y')
			self.description_dict[title]['date'] = description_date


