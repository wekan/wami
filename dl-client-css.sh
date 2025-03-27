#!/bin/bash

cd public

# Create directories first
mkdir -p packages/useraccounts_unstyled/lib/
mkdir -p packages/wekan-accounts-oidc/
mkdir -p packages/wekan-bootstrap-datepicker/bootstrap-datepicker/dist/css/
mkdir -p packages/mquandalle_jquery-textcomplete/jquery-textcomplete/dist/
mkdir -p packages/templates_tabs/
mkdir -p packages/wekan-fullcalendar/fullcalendar/
mkdir -p packages/wekan-fontawesome/fontawesome-free/css/
mkdir -p client/components/activities/
mkdir -p client/components/boards/
mkdir -p client/components/cards/
mkdir -p client/components/forms/
mkdir -p client/components/import/
mkdir -p client/components/lists/
mkdir -p client/components/main/
mkdir -p client/components/notifications/
mkdir -p client/components/rules/
mkdir -p client/components/settings/
mkdir -p client/components/sidebar/
mkdir -p client/components/swimlanes/
mkdir -p client/components/users/

# Download files
wget -P packages/useraccounts_unstyled/lib/ http://localhost:4000/packages/useraccounts_unstyled/lib/at_unstyled.css
wget -P packages/wekan-accounts-oidc/ http://localhost:4000/packages/wekan-accounts-oidc/oidc_login_button.css
wget -P packages/wekan-bootstrap-datepicker/bootstrap-datepicker/dist/css/ http://localhost:4000/packages/wekan-bootstrap-datepicker/bootstrap-datepicker/dist/css/bootstrap-datepicker3.css
wget -P packages/mquandalle_jquery-textcomplete/jquery-textcomplete/dist/ http://localhost:4000/packages/mquandalle_jquery-textcomplete/jquery-textcomplete/dist/jquery.textcomplete.css
wget -P packages/templates_tabs/ http://localhost:4000/packages/templates_tabs/templates_tabs.css
wget -P packages/wekan-fullcalendar/fullcalendar/ http://localhost:4000/packages/wekan-fullcalendar/fullcalendar/fullcalendar.css
wget -P packages/wekan-fontawesome/fontawesome-free/css/ http://localhost:4000/packages/wekan-fontawesome/fontawesome-free/css/all.css
wget -P packages/wekan-fontawesome/fontawesome-free/css/ http://localhost:4000/packages/wekan-fontawesome/fontawesome-free/css/brands.css
wget -P packages/wekan-fontawesome/fontawesome-free/css/ http://localhost:4000/packages/wekan-fontawesome/fontawesome-free/css/fontawesome.css
wget -P packages/wekan-fontawesome/fontawesome-free/css/ http://localhost:4000/packages/wekan-fontawesome/fontawesome-free/css/regular.css
wget -P packages/wekan-fontawesome/fontawesome-free/css/ http://localhost:4000/packages/wekan-fontawesome/fontawesome-free/css/solid.css
wget -P packages/wekan-fontawesome/fontawesome-free/css/ http://localhost:4000/packages/wekan-fontawesome/fontawesome-free/css/svg-with-js.css
wget -P packages/wekan-fontawesome/fontawesome-free/css/ http://localhost:4000/packages/wekan-fontawesome/fontawesome-free/css/v4-font-face.css
wget -P packages/wekan-fontawesome/fontawesome-free/css/ http://localhost:4000/packages/wekan-fontawesome/fontawesome-free/css/v4-shims.css
wget -P packages/wekan-fontawesome/fontawesome-free/css/ http://localhost:4000/packages/wekan-fontawesome/fontawesome-free/css/v5-font-face.css
wget -P client/components/activities/ http://localhost:4000/client/components/activities/activities.css
wget -P client/components/activities/ http://localhost:4000/client/components/activities/comments.css
wget -P client/components/boards/ http://localhost:4000/client/components/boards/boardBody.css
wget -P client/components/boards/ http://localhost:4000/client/components/boards/boardColors.css
wget -P client/components/boards/ http://localhost:4000/client/components/boards/boardHeader.css
wget -P client/components/boards/ http://localhost:4000/client/components/boards/boardsList.css
wget -P client/components/cards/ http://localhost:4000/client/components/cards/attachments.css
wget -P client/components/cards/ http://localhost:4000/client/components/cards/cardDate.css
wget -P client/components/cards/ http://localhost:4000/client/components/cards/cardDescription.css
wget -P client/components/cards/ http://localhost:4000/client/components/cards/cardDetails.css
wget -P client/components/cards/ http://localhost:4000/client/components/cards/cardTime.css
wget -P client/components/cards/ http://localhost:4000/client/components/cards/checklists.css
wget -P client/components/cards/ http://localhost:4000/client/components/cards/labels.css
wget -P client/components/cards/ http://localhost:4000/client/components/cards/minicard.css
wget -P client/components/cards/ http://localhost:4000/client/components/cards/resultCard.css
wget -P client/components/cards/ http://localhost:4000/client/components/cards/subtasks.css
wget -P client/components/forms/ http://localhost:4000/client/components/forms/datepicker.css
wget -P client/components/forms/ http://localhost:4000/client/components/forms/forms.css
wget -P client/components/import/ http://localhost:4000/client/components/import/import.css
wget -P client/components/lists/ http://localhost:4000/client/components/lists/list.css
wget -P client/components/main/ http://localhost:4000/client/components/main/accessibility.css
wget -P client/components/main/ http://localhost:4000/client/components/main/brokenCards.css
wget -P client/components/main/ http://localhost:4000/client/components/main/dueCards.css
wget -P client/components/main/ http://localhost:4000/client/components/main/editor.css
wget -P client/components/main/ http://localhost:4000/client/components/main/fonts.css
wget -P client/components/main/ http://localhost:4000/client/components/main/globalSearch.css
wget -P client/components/main/ http://localhost:4000/client/components/main/header.css
wget -P client/components/main/ http://localhost:4000/client/components/main/keyboardShortcuts.css
wget -P client/components/main/ http://localhost:4000/client/components/main/layouts.css
wget -P client/components/main/ http://localhost:4000/client/components/main/myCards.css
wget -P client/components/main/ http://localhost:4000/client/components/main/popup.css
wget -P client/components/main/ http://localhost:4000/client/components/main/spinner_bounce.css
wget -P client/components/main/ http://localhost:4000/client/components/main/spinner_cube.css
wget -P client/components/main/ http://localhost:4000/client/components/main/spinner_cube_grid.css
wget -P client/components/main/ http://localhost:4000/client/components/main/spinner_dot.css
wget -P client/components/main/ http://localhost:4000/client/components/main/spinner_double_bounce.css
wget -P client/components/main/ http://localhost:4000/client/components/main/spinner_rotateplane.css
wget -P client/components/main/ http://localhost:4000/client/components/main/spinner_scaleout.css
wget -P client/components/main/ http://localhost:4000/client/components/main/spinner_wave.css
wget -P client/components/notifications/ http://localhost:4000/client/components/notifications/notification.css
wget -P client/components/notifications/ http://localhost:4000/client/components/notifications/notifications.css
wget -P client/components/notifications/ http://localhost:4000/client/components/notifications/notificationsDrawer.css
wget -P client/components/rules/ http://localhost:4000/client/components/rules/rules.css
wget -P client/components/settings/ http://localhost:4000/client/components/settings/attachments.css
wget -P client/components/settings/ http://localhost:4000/client/components/settings/peopleBody.css
wget -P client/components/settings/ http://localhost:4000/client/components/settings/settingBody.css
wget -P client/components/settings/ http://localhost:4000/client/components/settings/settingHeader.css
wget -P client/components/settings/ http://localhost:4000/client/components/settings/translationBody.css
wget -P client/components/sidebar/ http://localhost:4000/client/components/sidebar/sidebar.css
wget -P client/components/sidebar/ http://localhost:4000/client/components/sidebar/sidebarSearches.css
wget -P client/components/swimlanes/ http://localhost:4000/client/components/swimlanes/swimlanes.css
wget -P client/components/users/ http://localhost:4000/client/components/users/userAvatar.css
wget -P client/components/users/ http://localhost:4000/client/components/users/userForm.css

cd ..
