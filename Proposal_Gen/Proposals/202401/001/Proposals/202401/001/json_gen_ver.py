#%%
import json
import os
import shutil

#%%

def save_to_json(data, output_file_path):
    with open(output_file_path, 'w') as output_file:
        json.dump(data, output_file, indent=2)

semester2code = { "sp":"01", "spr":"01", "spring":"01", "su":"02", "sum":"02", "summer":"02", "fa":"03", "fall":"03"}
thisfilename = os.path.basename(__file__) # should match _ver for version, ideally 3-digit string starting as "000", up to "999"

data_to_save = \
    {
        # -----------------------------------------------------------------------------------------------------------------------
        "Version":
            """001""",
        # -----------------------------------------------------------------------------------------------------------------------
        "Year":
            """2024""",
        # -----------------------------------------------------------------------------------------------------------------------
        "Semester":
            """Spring""",
        # -----------------------------------------------------------------------------------------------------------------------
        "project_name":
            """Mapping the Causes and Effects of Drone Strikes between Actors""",
        # -----------------------------------------------------------------------------------------------------------------------
        "Objective":
            """ 
            The goal of this project is to create visualizations and journalistic insights on the use of drone strikes across the world 
            in the last two decades. I plan on creating a set of visualizations that include an interactive website through which one 
            can see the location of each drone strike (through geospacial data) and a set of visualizations that denote the actors
            involved, civilian casualties, effectiveness of the strikes, and visualizations of the duration of conflicts by region.
            This project can be expanded to any other type of conflict including riots, war, etc.
            """,
        # -----------------------------------------------------------------------------------------------------------------------
        "Dataset":
            """
            I plan on using the ACLED dataset. I have been granted special access by the organization to download large amounts of 
            data, and I have created a dataset of 148,316 observations of drone strikes/aerial strikes since 1997. 
            The data can be accessed at https://acleddata.com/
            """,
        # -----------------------------------------------------------------------------------------------------------------------
        "Rationale":
            """
            This project will give more insight about the causes and effects of drone strikes in recent history. It will follow the
            taste of an investigative data journalism project through which stories about trends can be told. 
            """,
        # -----------------------------------------------------------------------------------------------------------------------
        "Approach":
            """
            I plan on approaching this capstone through several steps.  

            1. Exploratory Analysis
            2. Shiny R website  
            3. Create Individual Visualizations
            4. Conduct Text Analysis on the descriptions of the conflicts  
            """,
        # -----------------------------------------------------------------------------------------------------------------------
        "Timeline":
            """
            This a rough time line for this project:  

            - (1 Weeks) Exploratory analysis.  
            - (3 Weeks) Shiny R and Website  
            - (3 Weeks) Individual Visualizations  
            - (2 Weeks) Text Analysis  
            - (1 Weeks) Write Investigative Journalism Story with Findings and Visualizations
            - (1 Weeks) Final Paper
            - (1 Weeks) Poster board
            - (1 Weeks) Final Presentation  
            """,
        # -----------------------------------------------------------------------------------------------------------------------
        "Expected Number Students":
            """
            For this project maximum of 1 student can work on it.  
            """,
        # -----------------------------------------------------------------------------------------------------------------------
        "Possible Issues":
            """
            The challenge for me will be the text analysis, since that is something I have never done before.
            """,
        # -----------------------------------------------------------------------------------------------------------------------
        "Proposed by": "Jakob Aggers",
        "Proposed by email": "jakobaggers@gwu.edu",
        "instructor": "Edwin Lo",
        "instructor_email": "edwinlo@gwu.edu",
        "github_repo": "https://github.com/jakobaggers/24Spr_JAGGERS_projectName.git",
        # -----------------------------------------------------------------------------------------------------------------------
    }
os.makedirs(
    os.getcwd() + f'{os.sep}Proposals{os.sep}{data_to_save["Year"]}{semester2code[data_to_save["Semester"].lower()]}{os.sep}{data_to_save["Version"]}',
    exist_ok=True)
output_file_path = os.getcwd() + f'{os.sep}Proposals{os.sep}{data_to_save["Year"]}{semester2code[data_to_save["Semester"].lower()]}{os.sep}{data_to_save["Version"]}{os.sep}'
save_to_json(data_to_save, output_file_path + "input.json")
shutil.copy(thisfilename, output_file_path)
print(f"Data saved to {output_file_path}")
