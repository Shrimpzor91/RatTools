import time
import os
import shutil
import re
from watchdog.observers import Observer
from watchdog.events import FileSystemEventHandler

################## Name your Files and Folders #####################################

MedPC_Folder = r'C:\MED-PC\Data\Victor'
Backup1 = r'Y:\Victor\Backups FIles\OPD Backup Data files\Concatinated Backup File'
Backup2 = r'Y:\Victor\Backups FIles\OPD Backup Data files\OPD Data A'
MasterDataFile = 'OPD.txt'
NamingPattern = re.compile(r'OPD(\d+-\d+)_DelayTrain\(\)')



def on_file_created(event):
    if not event.is_directory:
        src_file_path = event.src_path
        file_name = os.path.basename(src_file_path)

        match = NamingPattern.match(file_name)
        if match:
            start_end_num = match.group(1)
            start_num, end_num = start_end_num.split('-')
            new_name = f"OPD{start_end_num}_DelayTrain()"
            new_path = os.path.join(MedPC_Folder, new_name)

            max_attempts = 5
            attempt = 0

            while attempt < max_attempts:
                try:
                    if os.path.exists(src_file_path):
                        os.rename(src_file_path, new_path)

                        shutil.copy2(new_path, Backup1)
                        shutil.copy2(new_path, Backup2)

                    break  # Successful renaming, exit the loop
                except PermissionError as e:
                    print(f"Attempt {attempt + 1}: PermissionError: {e}. Waiting...")
                    attempt += 1
                    time.sleep(2)  # Wait for 2 seconds before retrying

            if attempt == max_attempts:
                print(f"Failed to rename '{src_file_path}' after {max_attempts} attempts.")

####################################################################################

file_counters = {}
def get_next_number(file_pattern):
    if file_pattern not in file_counters:
        file_counters[file_pattern] = 100
    else:
        file_counters[file_pattern] += 1
    return file_counters[file_pattern]

if __name__ == "__main__":
    event_handler = FileSystemEventHandler()
    event_handler.on_created = on_file_created

    observer = Observer()
    observer.schedule(event_handler, path=MedPC_Folder, recursive=False)

    observer.start()

    try:
        while True:
            time.sleep(1)
    except KeyboardInterrupt:
        observer.stop()

    observer.join()
