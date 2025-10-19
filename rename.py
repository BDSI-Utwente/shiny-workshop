import os

def clean_filename(filename):
    # Split the filename into parts
    parts = filename.split('.')
    # Filter out parts containing "sync-conflict"
    clean_parts = [part for part in parts if 'sync-conflict' not in part]
    # Rejoin the parts with dots
    return '.'.join(clean_parts)

def rename_sync_conflict_files(directory='.'):
    # Walk through all directories and files
    for root, dirs, files in os.walk(directory):
        for filename in files:
            if 'sync-conflict' in filename:
                old_path = os.path.join(root, filename)
                new_filename = clean_filename(filename)
                new_path = os.path.join(root, new_filename)
                
                try:
                    # Only rename if the new filename is different
                    if new_filename != filename:
                        os.rename(old_path, new_path)
                        print(f'Renamed: {filename} -> {new_filename}')
                except OSError as e:
                    print(f'Error renaming {filename}: {e}')

if __name__ == '__main__':
    rename_sync_conflict_files()