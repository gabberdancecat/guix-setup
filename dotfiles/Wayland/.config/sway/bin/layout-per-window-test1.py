#!/usr/bin/env python

import sys
from typing import Optional
import i3ipc

# Define mappings of window classes/titles to specific layout indices
layout_mapping = {
    "firefox": 1,  # Example: Firefox windows switch to layout 1
    "code": 2,     # Example: VS Code windows switch to layout 2
}

def on_window_focus(ipc: i3ipc.connection.Connection, event: i3ipc.events.WindowEvent):
    global windows, prev_focused, default_layout

    # Get current layouts
    layouts = {
        input.identifier: input.xkb_active_layout_index for input in ipc.get_inputs()
    }

    # Save layouts for previous window
    windows[prev_focused] = layouts

    # Check if the focused window matches a specific class/title for layout switching
    focused_class = event.container.window_class
    focused_title = event.container.name
    target_layout = None

    # Check if there's a layout mapped for the class or title
    if focused_class in layout_mapping:
        target_layout = layout_mapping[focused_class]
    elif focused_title in layout_mapping:
        target_layout = layout_mapping[focused_title]

    if target_layout is not None:
        # Switch to the target layout if different from the current layout
        for kdb_id, layout_index in layouts.items():
            if layout_index != target_layout:
                ipc.command(f'input "{kdb_id}" xkb_switch_layout {target_layout}')
                break
    elif event.container.id in windows:
        # Restore layout of the newly focused known window if no specific layout is mapped
        for kdb_id, layout_index in windows[event.container.id].items():
            if layout_index != layouts[kdb_id]:
                ipc.command(f'input "{kdb_id}" xkb_switch_layout {layout_index}')
                break
    elif default_layout is not None:
        # Set default layout for a fresh window if no specific layout is mapped
        for kdb_id, layout_index in layouts.items():
            if layout_index != default_layout:
                ipc.command(f'input "{kdb_id}" xkb_switch_layout {default_layout}')
                break

    prev_focused = event.container.id
