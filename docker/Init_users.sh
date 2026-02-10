#!/bin/bash
USER_NAME="user"
echo "Se afecta solo el usuario ${USER_NAME}"

if ! id -u "${USER_NAME}" >/dev/null 2>&1; then
   echo "Usuario ${USER_NAME} no existe. Abortando."
   exit 1
fi

echo "Procesando usuario ${USER_NAME}"
src_base="/var/data/besmart/versiones"
dest_base="/home/${USER_NAME}/Documents/besmart"
mkdir -p "$dest_base"

copy_version() {
   local version="$1"
   local orig_version_path="$src_base/$version"
   local dest_path="$dest_base/$version"

   if [ ! -d "$orig_version_path" ]; then
      echo "No existe la versión $version en $src_base"
      return 1
   fi

   echo "Copiando $orig_version_path en $dest_path"
   /bin/cp -rf "$orig_version_path/." "$dest_path"

   mkdir -p "$dest_path/Logs"
   mkdir -p "$dest_path/Trabajo"
   mkdir -p "$dest_path/Reportes"
   mkdir -p "$dest_path/Auxil"
}

if [ -n "$BSM_VERSION" ]; then
   copy_version "$BSM_VERSION"
else
   for dir_start in "$src_base"/*; do
      version=$(basename "$dir_start")
      copy_version "$version"
   done
fi
   # Create .bashrc for non-login shells (Positron, etc.)
   bashrc_file="/home/${USER_NAME}/.bashrc"
   if [ ! -f "$bashrc_file" ]; then
      cat > "$bashrc_file" <<'EOF'
# SmartModel environment setup
if [ -n "$BSM_DIR" ] && [ -d "$BSM_DIR" ]; then
    cd "$BSM_DIR"
fi
EOF
   fi

   # Create .bash_profile for SSH login shells to set up BSM environment
   bash_profile="/home/${USER_NAME}/.bash_profile"
   if [ ! -f "$bash_profile" ]; then
      cat > "$bash_profile" <<'EOF'
# Source .bashrc if it exists
if [ -f ~/.bashrc ]; then
    . ~/.bashrc
fi
EOF
   fi

   # Ajusta los permisos desde cada user home.
   chown -R $(id -un "${USER_NAME}"):$(id -gn "${USER_NAME}") "/home/${USER_NAME}"
   chmod 0775 "/home/${USER_NAME}"
