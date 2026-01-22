#!/bin/bash
# get list of all users
echo "Se afectan todos los usuarios con 1000 <=UID < 2000"
_USERS="$(awk -F: '/\/home/ && ($3 >= 1000) && ($3 < 2000) {print $1}' /etc/passwd)"
for u in $_USERS
do
   echo "Procesando usuario $u"
   dir_dest="/home/${u}/Documents/besmart"
   mkdir -p "$dir_dest"
   for dir_start in /var/data/besmart/versiones/*
   do
      version=$(basename "$dir_start")
      echo "La versión es $version"
      orig_path="/var/data/besmart/versiones/$version/Ej_Inicial"
      dest_path="$dir_dest/$version"
      echo "Copiando $orig_path en $dest_path"
      /bin/cp -rf "$orig_path" "$dest_path"
   done
   # Create .bashrc for non-login shells (Positron, etc.)
   bashrc_file="/home/${u}/.bashrc"
   if [ ! -f "$bashrc_file" ]; then
      cat > "$bashrc_file" <<'EOF'
# SmartModel environment setup
if [ -n "$BSM_DIR" ] && [ -d "$BSM_DIR" ]; then
    cd "$BSM_DIR"
fi
EOF
   fi

   # Create .bash_profile for SSH login shells to set up BSM environment
   bash_profile="/home/${u}/.bash_profile"
   if [ ! -f "$bash_profile" ]; then
      cat > "$bash_profile" <<'EOF'
# Source .bashrc if it exists
if [ -f ~/.bashrc ]; then
    . ~/.bashrc
fi
EOF
   fi

   # Ajusta los permisos desde cada user home.
   chown -R $(id -un $u):$(id -gn $u) "/home/${u}"
   chmod 0775 "/home/${u}"
done
