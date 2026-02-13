import gftp/file_type
import gftp/internal/command.{
  Abor, Appe, Auth, Cdup, ClearCommandChannel, Custom, Cwd, Dele, Eprt, Epsv,
  Feat, List, Mdtm, Mkd, Mlsd, Mlst, Nlst, Noop, Opts, Pass, Pasv, Pbsz, Port,
  Prot, Pwd, Quit, RenameFrom, RenameTo, Rest, Retr, Rmd, Site, Size, Stor, Type,
  User,
}
import gftp/internal/command/protection_level
import gftp/mode.{V4, V6}
import gleam/option.{None, Some}

pub fn encode_abor_test() {
  let assert "ABOR" = command.to_string(Abor)
}

pub fn encode_appe_test() {
  let assert "APPE upload.txt" = command.to_string(Appe("upload.txt"))
}

pub fn encode_auth_test() {
  let assert "AUTH TLS" = command.to_string(Auth)
}

pub fn encode_ccc_test() {
  let assert "CCC" = command.to_string(ClearCommandChannel)
}

pub fn encode_cdup_test() {
  let assert "CDUP" = command.to_string(Cdup)
}

pub fn encode_cwd_test() {
  let assert "CWD /home/user" = command.to_string(Cwd("/home/user"))
}

pub fn encode_dele_test() {
  let assert "DELE old_file.txt" = command.to_string(Dele("old_file.txt"))
}

pub fn encode_eprt_v4_test() {
  let assert "EPRT |1|192.168.1.1|21|" =
    command.to_string(Eprt("192.168.1.1", 21, V4))
}

pub fn encode_eprt_v6_test() {
  let assert "EPRT |2|::1|21|" = command.to_string(Eprt("::1", 21, V6))
}

pub fn encode_epsv_test() {
  let assert "EPSV" = command.to_string(Epsv)
}

pub fn encode_feat_test() {
  let assert "FEAT" = command.to_string(Feat)
}

pub fn encode_list_with_path_test() {
  let assert "LIST /home" = command.to_string(List(Some("/home")))
}

pub fn encode_list_without_path_test() {
  let assert "LIST" = command.to_string(List(None))
}

pub fn encode_mdtm_test() {
  let assert "MDTM file.txt" = command.to_string(Mdtm("file.txt"))
}

pub fn encode_mlsd_with_path_test() {
  let assert "MLSD /data" = command.to_string(Mlsd(Some("/data")))
}

pub fn encode_mlsd_without_path_test() {
  let assert "MLSD" = command.to_string(Mlsd(None))
}

pub fn encode_mlst_with_path_test() {
  let assert "MLST file.txt" = command.to_string(Mlst(Some("file.txt")))
}

pub fn encode_mlst_without_path_test() {
  let assert "MLST" = command.to_string(Mlst(None))
}

pub fn encode_mkd_test() {
  let assert "MKD new_dir" = command.to_string(Mkd("new_dir"))
}

pub fn encode_nlst_with_path_test() {
  let assert "NLST /files" = command.to_string(Nlst(Some("/files")))
}

pub fn encode_nlst_without_path_test() {
  let assert "NLST" = command.to_string(Nlst(None))
}

pub fn encode_noop_test() {
  let assert "NOOP" = command.to_string(Noop)
}

pub fn encode_opts_with_options_test() {
  let assert "OPTS UTF8 ON" = command.to_string(Opts("UTF8", Some("ON")))
}

pub fn encode_opts_without_options_test() {
  let assert "OPTS UTF8" = command.to_string(Opts("UTF8", None))
}

pub fn encode_pass_test() {
  let assert "PASS secret" = command.to_string(Pass("secret"))
}

pub fn encode_pasv_test() {
  let assert "PASV" = command.to_string(Pasv)
}

pub fn encode_pbsz_test() {
  let assert "PBSZ 0" = command.to_string(Pbsz(0))
}

pub fn encode_port_test() {
  let assert "PORT 192,168,1,1,4,1" = command.to_string(Port("192,168,1,1,4,1"))
}

pub fn encode_prot_clear_test() {
  let assert "PROT C" = command.to_string(Prot(protection_level.Clear))
}

pub fn encode_prot_private_test() {
  let assert "PROT P" = command.to_string(Prot(protection_level.Private))
}

pub fn encode_pwd_test() {
  let assert "PWD" = command.to_string(Pwd)
}

pub fn encode_quit_test() {
  let assert "QUIT" = command.to_string(Quit)
}

pub fn encode_rename_from_test() {
  let assert "RNFR old.txt" = command.to_string(RenameFrom("old.txt"))
}

pub fn encode_rename_to_test() {
  let assert "RNTO new.txt" = command.to_string(RenameTo("new.txt"))
}

pub fn encode_rest_test() {
  let assert "REST 1024" = command.to_string(Rest(1024))
}

pub fn encode_retr_test() {
  let assert "RETR download.txt" = command.to_string(Retr("download.txt"))
}

pub fn encode_rmd_test() {
  let assert "RMD old_dir" = command.to_string(Rmd("old_dir"))
}

pub fn encode_site_test() {
  let assert "SITE CHMOD 755 file.txt" =
    command.to_string(Site("CHMOD 755 file.txt"))
}

pub fn encode_size_test() {
  let assert "SIZE file.bin" = command.to_string(Size("file.bin"))
}

pub fn encode_store_test() {
  let assert "STOR upload.bin" = command.to_string(Stor("upload.bin"))
}

pub fn encode_type_ascii_default_test() {
  let assert "TYPE A N" =
    command.to_string(Type(file_type.Ascii(file_type.Default)))
}

pub fn encode_type_image_test() {
  let assert "TYPE I" = command.to_string(Type(file_type.Image))
}

pub fn encode_type_binary_test() {
  let assert "TYPE I" = command.to_string(Type(file_type.Binary))
}

pub fn encode_user_test() {
  let assert "USER anonymous" = command.to_string(User("anonymous"))
}

pub fn encode_custom_test() {
  let assert "XCRC file.txt" = command.to_string(Custom("XCRC file.txt"))
}
