rm lib/libglui.a
#g++-mp-4.4 -I./ -I./include -D__APPLE__ -g -O2 -Wall -c glui_add_controls.cpp
#g++-mp-4.4 -I./ -I./include -D__APPLE__ -g -O2 -Wall -c glui_string.cpp
#g++-mp-4.4 -I./ -I./include -D__APPLE__ -g -O2 -Wall -c glui.cpp
#g++-mp-4.4 -I./ -I./include -D__APPLE__ -g -O2 -Wall -c glui_bitmap_img_data.cpp
#g++-mp-4.4 -I./ -I./include -D__APPLE__ -g -O2 -Wall -c glui_bitmaps.cpp
#g++-mp-4.4 -I./ -I./include -D__APPLE__ -g -O2 -Wall -c glui_button.cpp
#g++-mp-4.4 -I./ -I./include -D__APPLE__ -g -O2 -Wall -c glui_edittext.cpp

#g++-mp-4.4 -I./ -I./include -D__APPLE__ -g -O2 -Wall -c glui_commandline.cpp
#g++-mp-4.4 -I./ -I./include -D__APPLE__ -g -O2 -Wall -c glui_checkbox.cpp
#g++-mp-4.4 -I./ -I./include -D__APPLE__ -g -O2 -Wall -c glui_node.cpp
#g++-mp-4.4 -I./ -I./include -D__APPLE__ -g -O2 -Wall -c glui_radio.cpp
#g++-mp-4.4 -I./ -I./include -D__APPLE__ -g -O2 -Wall -c glui_statictext.cpp
#g++-mp-4.4 -I./ -I./include -D__APPLE__ -g -O2 -Wall -c glui_panel.cpp
#g++-mp-4.4 -I./ -I./include -D__APPLE__ -g -O2 -Wall -c glui_separator.cpp
#g++-mp-4.4 -I./ -I./include -D__APPLE__ -g -O2 -Wall -c glui_spinner.cpp

g++-mp-4.4 -I./ -I./include -D__APPLE__ -g -O0 -Wall -c glui_control.cpp
#g++-mp-4.4 -I./ -I./include -D__APPLE__ -g -O2 -Wall -c glui_column.cpp
#g++-mp-4.4 -I./ -I./include -D__APPLE__ -g -O2 -Wall -c glui_translation.cpp
#g++-mp-4.4 -I./ -I./include -D__APPLE__ -g -O2 -Wall -c glui_rotation.cpp
#g++-mp-4.4 -I./ -I./include -D__APPLE__ -g -O2 -Wall -c glui_mouse_iaction.cpp
#g++-mp-4.4 -I./ -I./include -D__APPLE__ -g -O2 -Wall -c glui_listbox.cpp
#g++-mp-4.4 -I./ -I./include -D__APPLE__ -g -O2 -Wall -c glui_rollout.cpp
#g++-mp-4.4 -I./ -I./include -D__APPLE__ -g -O2 -Wall -c glui_window.cpp
#g++-mp-4.4 -I./ -I./include -D__APPLE__ -g -O2 -Wall -c arcball.cpp
#g++-mp-4.4 -I./ -I./include -D__APPLE__ -g -O2 -Wall -c algebra3.cpp
#g++-mp-4.4 -I./ -I./include -D__APPLE__ -g -O2 -Wall -c quaternion.cpp

#g++-mp-4.4 -I./ -I./include -D__APPLE__ -g -O2 -Wall -c viewmodel.cpp
g++-mp-4.4 -I./ -I./include -D__APPLE__ -g -O0 -Wall -c glui_treepanel.cpp
#g++-mp-4.4 -I./ -I./include -D__APPLE__ -g -O2 -Wall -c glui_tree.cpp
#g++-mp-4.4 -I./ -I./include -D__APPLE__ -g -O2 -Wall -c glui_textbox.cpp
#g++-mp-4.4 -I./ -I./include -D__APPLE__ -g -O2 -Wall -c glui_scrollbar.cpp
#g++-mp-4.4 -I./ -I./include -D__APPLE__ -g -O2 -Wall -c glui_list.cpp
make
g++-mp-4.4 -I./ -I./include -D__APPLE__ -g -O0 -Wall -o bin/example6 example/example6.cpp  -L./lib -lglui -framework glut -framework OpenGL -framework Cocoa 
bin/example6

