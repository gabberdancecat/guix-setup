#!/usr/bin/env python3
import i3ipc
import sys

def main():
    # Connect to sway/i3
    sway = i3ipc.Connection()
    
    # Iterate through workspaces
    for workspace in [ws for output in sway.get_tree().nodes for ws in output.nodes]:
        # Find focused window
        focus = workspace.find_focused()
        if focus is None:
            continue
            
        # Get all named windows in the workspace
        descendants = [d for d in workspace.descendants() if d.name is not None]
        
        # Find index of focused window
        current_index = descendants.index(focus)
        
        # Calculate target index based on direction
        if sys.argv[1] == "next":
            target_index = (current_index + 1) % len(descendants)
        else:  # prev
            target_index = (current_index - 1) % len(descendants)
            
        # Get the target window
        target_window = descendants[target_index]
        
        # Perform the swap
        # The mark command helps us keep track of the windows during the swap
        sway.command(f'[con_id={focus.id}] mark --add "_swap_source"')
        sway.command(f'[con_id={target_window.id}] mark --add "_swap_target"')
        
        # Swap the windows
        sway.command('[con_mark="_swap_source"] swap container with mark "_swap_target"')
        
        # Clean up the marks
        sway.command('[con_mark="_swap_source"] unmark "_swap_source"')
        sway.command('[con_mark="_swap_target"] unmark "_swap_target"')
        
        # Keep focus on the original window (now in new position)
        sway.command(f'[con_id={focus.id}] focus')
        
        sys.exit()

if __name__ == '__main__':
    main()
