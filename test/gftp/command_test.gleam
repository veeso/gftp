import gftp/command.{
  Abor, Appe, Auth, Cdup, ClearCommandChannel, Custom, Cwd, Dele, Eprt, Epsv,
  Feat, List, Mdtm, Mkd, Mlsd, Mlst, Nlst, Noop, Opts, Pass, Pasv, Pbsz, Port,
  Prot, Pwd, Quit, RenameFrom, RenameTo, Rest, Retr, Rmd, Site, Size, Stor, Type,
  User, V4, V6,
}
import gftp/command/file_type
import gftp/command/protection_level
import gleam/option.{None, Some}
import gleeunit/should

pub fn encode_abor_test() {
  command.to_string(Abor)
  |> should.equal("ABOR")
}

pub fn encode_appe_test() {
  command.to_string(Appe("upload.txt"))
  |> should.equal("APPE upload.txt")
}

pub fn encode_auth_test() {
  command.to_string(Auth)
  |> should.equal("AUTH TLS")
}

pub fn encode_ccc_test() {
  command.to_string(ClearCommandChannel)
  |> should.equal("CCC")
}

pub fn encode_cdup_test() {
  command.to_string(Cdup)
  |> should.equal("CDUP")
}

pub fn encode_cwd_test() {
  command.to_string(Cwd("/home/user"))
  |> should.equal("CWD /home/user")
}

pub fn encode_dele_test() {
  command.to_string(Dele("old_file.txt"))
  |> should.equal("DELE old_file.txt")
}

pub fn encode_eprt_v4_test() {
  command.to_string(Eprt("192.168.1.1", 21, V4))
  |> should.equal("EPRT |1|192.168.1.1|21|")
}

pub fn encode_eprt_v6_test() {
  command.to_string(Eprt("::1", 21, V6))
  |> should.equal("EPRT |2|::1|21|")
}

pub fn encode_epsv_test() {
  command.to_string(Epsv)
  |> should.equal("EPSV")
}

pub fn encode_feat_test() {
  command.to_string(Feat)
  |> should.equal("FEAT")
}

pub fn encode_list_with_path_test() {
  command.to_string(List(Some("/home")))
  |> should.equal("LIST /home")
}

pub fn encode_list_without_path_test() {
  command.to_string(List(None))
  |> should.equal("LIST")
}

pub fn encode_mdtm_test() {
  command.to_string(Mdtm("file.txt"))
  |> should.equal("MDTM file.txt")
}

pub fn encode_mlsd_with_path_test() {
  command.to_string(Mlsd(Some("/data")))
  |> should.equal("MLSD /data")
}

pub fn encode_mlsd_without_path_test() {
  command.to_string(Mlsd(None))
  |> should.equal("MLSD")
}

pub fn encode_mlst_with_path_test() {
  command.to_string(Mlst(Some("file.txt")))
  |> should.equal("MLST file.txt")
}

pub fn encode_mlst_without_path_test() {
  command.to_string(Mlst(None))
  |> should.equal("MLST")
}

pub fn encode_mkd_test() {
  command.to_string(Mkd("new_dir"))
  |> should.equal("MKD new_dir")
}

pub fn encode_nlst_with_path_test() {
  command.to_string(Nlst(Some("/files")))
  |> should.equal("NLST /files")
}

pub fn encode_nlst_without_path_test() {
  command.to_string(Nlst(None))
  |> should.equal("NLST")
}

pub fn encode_noop_test() {
  command.to_string(Noop)
  |> should.equal("NOOP")
}

pub fn encode_opts_with_options_test() {
  command.to_string(Opts("UTF8", Some("ON")))
  |> should.equal("OPTS UTF8 ON")
}

pub fn encode_opts_without_options_test() {
  command.to_string(Opts("UTF8", None))
  |> should.equal("OPTS UTF8")
}

pub fn encode_pass_test() {
  command.to_string(Pass("secret"))
  |> should.equal("PASS secret")
}

pub fn encode_pasv_test() {
  command.to_string(Pasv)
  |> should.equal("PASV")
}

pub fn encode_pbsz_test() {
  command.to_string(Pbsz(0))
  |> should.equal("PBSZ 0")
}

pub fn encode_port_test() {
  command.to_string(Port("192,168,1,1,4,1"))
  |> should.equal("PORT 192,168,1,1,4,1")
}

pub fn encode_prot_clear_test() {
  command.to_string(Prot(protection_level.Clear))
  |> should.equal("PROT C")
}

pub fn encode_prot_private_test() {
  command.to_string(Prot(protection_level.Private))
  |> should.equal("PROT P")
}

pub fn encode_pwd_test() {
  command.to_string(Pwd)
  |> should.equal("PWD")
}

pub fn encode_quit_test() {
  command.to_string(Quit)
  |> should.equal("QUIT")
}

pub fn encode_rename_from_test() {
  command.to_string(RenameFrom("old.txt"))
  |> should.equal("RNFR old.txt")
}

pub fn encode_rename_to_test() {
  command.to_string(RenameTo("new.txt"))
  |> should.equal("RNTO new.txt")
}

pub fn encode_rest_test() {
  command.to_string(Rest(1024))
  |> should.equal("REST 1024")
}

pub fn encode_retr_test() {
  command.to_string(Retr("download.txt"))
  |> should.equal("RETR download.txt")
}

pub fn encode_rmd_test() {
  command.to_string(Rmd("old_dir"))
  |> should.equal("RMD old_dir")
}

pub fn encode_site_test() {
  command.to_string(Site("CHMOD 755 file.txt"))
  |> should.equal("SITE CHMOD 755 file.txt")
}

pub fn encode_size_test() {
  command.to_string(Size("file.bin"))
  |> should.equal("SIZE file.bin")
}

pub fn encode_store_test() {
  command.to_string(Stor("upload.bin"))
  |> should.equal("STOR upload.bin")
}

pub fn encode_type_ascii_default_test() {
  command.to_string(Type(file_type.Ascii(file_type.Default)))
  |> should.equal("TYPE A N")
}

pub fn encode_type_image_test() {
  command.to_string(Type(file_type.Image))
  |> should.equal("TYPE I")
}

pub fn encode_type_binary_test() {
  command.to_string(Type(file_type.Binary))
  |> should.equal("TYPE I")
}

pub fn encode_user_test() {
  command.to_string(User("anonymous"))
  |> should.equal("USER anonymous")
}

pub fn encode_custom_test() {
  command.to_string(Custom("XCRC file.txt"))
  |> should.equal("XCRC file.txt")
}
