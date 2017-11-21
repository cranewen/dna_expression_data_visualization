import os
import re, csv, sys
import os.path
from numpy.core.defchararray import upper
import pandas as pd
import pprint
from collections import defaultdict
import sqlalchemy
from sqlalchemy import create_engine
from sqlalchemy.ext.declarative import declarative_base
from sqlalchemy.orm import sessionmaker, relationship, backref
from sqlalchemy import func
from datetime import datetime
sys.path.append(os.path.join(os.path.dirname(__file__), '..'))
from Module.createTables import *


class GeneExpressionParser:
	def __init__(self, path):
		self.path = path

	# Get all the gene files with full path and file name for preparing data parsing.
	def getFullPathFileNames(self):
		fullPathFileNames = []

		for dirNames in os.walk(self.path):
			if (dirNames[1] == []):
				for fileName in dirNames[2]:
					fullPathFileNames.append(dirNames[0] + '/' + fileName)
		return fullPathFileNames

	def getGeneName(self,s):
		subString = re.search(r"(s|Data 3-)(.+?)(.csv)", s).group(2)
		return subString

	# Read all the files and giving {Experiment:{GeneName:{data}} as dictionary structure
	def getDataDict(self):
		experiment_dict = defaultdict(dict)
		genes_dict = defaultdict(dict)
		files = self.getFullPathFileNames()

		for f in files:
			description = f.split('/')[-2]
			geneName = self.getGeneName(f.split('/')[-1])
			geneExpressionData = pd.read_csv(f)

			experiment_dict[description][geneName] = geneExpressionData

		return experiment_dict



class FinalGeneExpressionDict:
	def __init__(self, path):
		self.path = path
		self.experimentDict = GeneExpressionParser(self.path).getDataDict()

	def connectDB(self):
	    try:
	        engine = create_engine("mysql+pymysql://db:ps@127.0.0.1:3306/b_dna")
	    except sqlalchemy.exc.DatabaseError:
	        print "Can't connect mysql."
	    return engine

	def createSession(self):
	    engine = self.connectDB()
	    Session = sessionmaker(bind=engine)
	    session = Session()
	    return session

	# Get gene expression dictionary from geneExpressionParser, and then turn all fields into IDs as FK by selecting from the database.
	# Therefore, inserting all the data straight into tables from this dictionary.
	def getFinalGeneExpressionDict(self):
		finallGeneExpressionDict = defaultdict(dict)
		session = self.createSession()
		for description, geneDict in self.experimentDict.iteritems():
			experiementID = session.query(Experiment.id).filter(Experiment.description == description).one()[0]

			for gene, geneData in geneDict.iteritems():
				geneID = session.query(GeneAlias.gene_id).filter(GeneAlias.alias_name == func.binary(gene)).one()[0]

				conditionIDs = []
				for condition in geneData.columns:
					conditionID = session.query(Condition.id).filter(Condition.name == condition.strip()).one()[0]
					conditionIDs.append(conditionID)
				# reassign values to columns will change dataframe's columns' names
				geneData.columns = conditionIDs

				finallGeneExpressionDict[experiementID][geneID] = geneData
					# print condition
		session.close()
		return finallGeneExpressionDict
