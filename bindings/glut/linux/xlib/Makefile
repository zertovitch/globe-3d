#
# Makefile for Ada binding to Motif/X
#
# THIS MAKEFILE IS FOR GNAT!
#
# 5.10.96 H.-F. Vogt
# 17.5.97 changed for Version 0.3
# 14.9.97 changed for Version 0.4
#  1.1.98 changed for Version 0.5
# 20.6.98 changed for Version 0.7
# 26 Aug 2001 changed for Version 0.7.2
#

include ../Local.conf

GENERAL_SRC = string_list.ads string_list_conversion.adb x_command_line.adb \
              x_command_line-internal.adb x_strings.adb x_wide_strings.adb \
              x_connection.ads

X_BIND_SRC = x_lib-predefined_atoms.ads x_lib-cursor.adb x_lib-screen_saver.ads \
             x_lib-resource.adb x_lib-resource-internal.adb \
             x_lib.adb x_lib-tasking.adb x_lib-host_access.adb \
             x_lib-key_syms.adb \
             x_lib-key_syms-iso9995.ads x_lib-key_syms-latin1.ads \
             x_lib-std_colormap.adb x_lib-property.adb x_lib-extensions.adb

XAU_BIND_SRC = x_auth.adb

XMU_BIND_SRC = x_misc_utils.adb

XT_BIND_SRC = resource_strings.adb x_toolkit.adb x_toolkit-internal.adb \
              x_toolkit-resource.adb x_toolkit-shell.ads

XM_BIND_SRC = xm_widgets.adb xm_widgets-atom_manager.ads xm_widgets-clipboard.adb \
              xm_widgets-display.adb xm_widgets-drag_context.adb xm_widgets-drag_icon.adb \
              xm_widgets-drag_over_shell.ads xm_widgets-drop_site_manager.adb \
              xm_widgets-drop_transfer.adb xm_widgets-transfer.adb \
              xm_widgets-manager.adb \
              xm_widgets-manager-bulletin_board.adb \
              xm_widgets-manager-bulletin_board-form.adb xm_widgets-manager-bulletin_board-message_box.adb \
              xm_widgets-manager-bulletin_board-selection_box.adb \
              xm_widgets-manager-bulletin_board-selection_box-command.adb \
              xm_widgets-manager-bulletin_board-selection_box-file_selection_box.adb \
              xm_widgets-manager-drawing_area.adb \
              xm_widgets-manager-frame.adb xm_widgets-manager-paned_window.adb \
              xm_widgets-manager-row_column.adb xm_widgets-manager-row_column-simple.adb \
              xm_widgets-manager-scale.adb \
              xm_widgets-manager-scrolled_window.adb \
              xm_widgets-manager-scrolled_window-main_window.adb
XM_BIND_SRC2 = xm_widgets-primitive.adb \
               xm_widgets-primitive-arrow_button.adb \
               xm_widgets-primitive-label.adb \
               xm_widgets-primitive-label-cascade_button.adb xm_widgets-primitive-label-drawn_button.adb \
               xm_widgets-primitive-label-push_button.adb xm_widgets-primitive-label-toggle_button.adb \
               xm_widgets-primitive-list.adb \
               xm_widgets-primitive-scroll_bar.adb xm_widgets-primitive-separator.adb \
               xm_widgets-primitive-text.adb \
               xm_widgets-protocols.adb xm_widgets-screen.adb xm_widgets-shell.adb

# source for Motif2.x
XM2X_BIND_SRC = xm_widgets-manager-combo_box.adb xm_widgets-manager-container.adb \
               xm_widgets-manager-notebook.adb xm_widgets-manager-spin_box.adb \
               xm_widgets-manager-spin_box-simple.adb \
               xm_widgets-primitive-icon.adb \
               xm_widgets-print.adb

# source solely for Motif2.0
XM20_BIND_SRC = xm_widgets-primitive-cs_text.adb

XPM_BIND_SRC = xpm_lib.adb

X63_BIND_SRC = x_lib-extensions-print.adb

# if package body exists, compile it, else take the specification
all: i-cwstri.o i-csstli.o
	@for i in $(GENERAL_SRC); \
	do \
		$(ADA_MAKE) -c $(ADA_MAKE_FLAGS) $$i ;\
	done
	@for i in $(X_BIND_SRC); \
	do \
		$(ADA_MAKE) -c $(ADA_MAKE_FLAGS) $$i ;\
	done
	@if test -n "$(IS_X11R63)" ; \
	then \
		for i in $(X63_BIND_SRC); \
		do \
			$(ADA_MAKE) -c $(ADA_MAKE_FLAGS) $$i ;\
		done; \
	fi
	@for i in $(XAU_BIND_SRC); \
	do \
		$(ADA_MAKE) -c $(ADA_MAKE_FLAGS) $$i ;\
	done
	@for i in $(XMU_BIND_SRC); \
	do \
		$(ADA_MAKE) -c $(ADA_MAKE_FLAGS) $$i ;\
	done
	@for i in $(XT_BIND_SRC); \
	do \
		$(ADA_MAKE) -c $(ADA_MAKE_FLAGS) $$i ;\
	done
	@for i in $(XM_BIND_SRC); \
	do \
		$(ADA_MAKE) -c $(ADA_MAKE_FLAGS) $$i ;\
	done
	@for i in $(XM_BIND_SRC2); \
	do \
		$(ADA_MAKE) -c $(ADA_MAKE_FLAGS) $$i ;\
	done
	@if test -n "$(IS_MOTIF21)" -o -n "$(IS_MOTIF20)" ; \
	then \
		for i in $(XM2X_BIND_SRC); \
		do \
			$(ADA_MAKE) -c $(ADA_MAKE_FLAGS) $$i ;\
		done; \
		if test -n "$(IS_MOTIF20)" ; \
		then \
			for i in $(XM20_BIND_SRC); \
			do \
				$(ADA_MAKE) -c $(ADA_MAKE_FLAGS) $$i ;\
			done; \
		fi \
	fi
	@if test -n "$(HAS_XPM)" ; \
	then \
		for i in $(XPM_BIND_SRC); \
		do \
			$(ADA_MAKE) -c $(ADA_MAKE_FLAGS) $$i ;\
		done \
	fi

i-cwstri.o: i-cwstri.ads i-cwstri.adb
	$(ADA_COMP) $(ADA_CFLAGS) -gnatg i-cwstri.adb

i-csstli.o: i-csstli.ads i-csstli.adb
	$(ADA_COMP) $(ADA_CFLAGS) -gnatg i-csstli.adb

clean:

distclean:
	$(RM) *.ali *.o
