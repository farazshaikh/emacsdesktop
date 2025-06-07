# Remove any previously installed versions if they exist
sudo rm -rf ./squashfs-root || true
sudo rm -rf /opt/cursor || true
sudo rm -f /usr/share/applications/cursor.desktop || true
sudo rm -f cursor-*.AppImage

# Download the latest version
curl -JLO https://downloader.cursor.sh/linux/appImage/x64

# Extract AppImage and fix permissions
chmod +x ./cursor-*.AppImage           # Make executable
./cursor-*.AppImage --appimage-extract # Extract appimage
rm ./cursor-*.AppImage
sudo mv ./squashfs-root /opt/cursor                 # Move extracted image to program directory
sudo chown -R root: /opt/cursor                     # Change owner to root
sudo chmod 4755 /opt/cursor/chrome-sandbox          # Fix permissions for the sandbox file
sudo find /opt/cursor -type d -exec chmod 755 {} \; # Some directories are only accessible by root which prevents application from launching. Lets fix
sudo chmod 644 /opt/cursor/cursor.png               # Make sure the app icon has permission to be viewed

# Create Desktop entry
cat >cursor.desktop <<EOL
[Desktop Entry]
Name=Cursor AI IDE
Exec=/opt/cursor/AppRun
Icon=/opt/cursor/cursor.png
Type=Application
Categories=Development;
EOL

# Set permisisons for .desktop file and move it to the correct directory
sudo chown root: cursor.desktop
sudo chmod 644 cursor.desktop
sudo mv cursor.desktop /usr/share/applications
