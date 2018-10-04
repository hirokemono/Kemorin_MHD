!>@file   set_parallel_file_name.f90
!!@brief  module set_parallel_file_name
!!
!!@author H. Matsui
!!@date   Programmed  H. Matsui in 2004
!
!>@brief  Set file extension
!!
!!@verbatim
!!      character(len = kchara) function delete_directory_name          &
!!     &                               (dir_file_name)
!!
!!      character(len = kchara) function add_int_suffix                 &
!!     &                               (int_id, file_header)
!!
!!      character(len = kchara) function add_dat_extension(file_header)
!!                put ".dat" at the end
!!
!!      subroutine add_pvtk_extension(file_header, file_name)
!!                put ".pvtk" at the end
!!
!!      subroutine add_hdf_extension(file_header, file_name)
!!                put ".h5" at the end
!!      subroutine add_xdmf_extension(file_header, file_name)
!!                put ".xdmf" at the end
!!      subroutine add_mesh_suffix(file_header, file_name)
!!                put ".mesh" at the end
!!      subroutine add_field_suffix(file_header, file_name)
!!                put ".field" at the end
!!      subroutine add_xdmf_suffix(file_header, file_name)
!!                put ".solution" at the end
!!
!!      subroutine add_dx_extension(file_header, file_name)
!!                put ".dx" at the end
!!      subroutine add_node_extension(file_header, file_name)
!!                put ".node.dat" at the end
!!      subroutine add_connect_extension(file_header, file_name)
!!                put ".connect.dat" at the end
!!
!!      character(len = kchara) function add_fld_extension(file_header)
!!                put ".fld" at the end
!!      subroutine add_flb_extension(file_header, file_name)
!!                put ".flb" at the end
!!      subroutine add_fst_extension(file_header, file_name)
!!                put ".fst" at the end
!!      subroutine add_fsb_extension(file_header, file_name)
!!                put ".fsb"
!!      subroutine add_elaps_postfix(file_header, file_name)
!!                put ".elps" at the end
!!
!!      character(len = kchara) function add_gzip_extension(file_header)
!!                put ".gz"
!!      character(len = kchara) function add_null_character(file_header)
!!                put null character at the end
!!
!!      subroutine add_ksm_extension(file_header, file_name)
!!                put ".ksm" at the end
!!
!!      subroutine add_gfm_extension(file_header, file_name)
!!                put ".gfm" at the end
!!      subroutine add_gfb_extension(file_header, file_name)
!!                put ".gfb" at the end
!!
!!      subroutine add_gel_extension(file_header, file_name)
!!                put ".gel" at the end
!!      subroutine add_gsf_extension(file_header, file_name)
!!                put ".gsf" at the end
!!      subroutine add_ged_extension(file_header, file_name)
!!                put ".ged" at the end
!!      subroutine add_elb_extension(file_header, file_name)
!!                put ".elb" at the end
!!      subroutine add_sfb_extension(file_header, file_name)
!!                put ".sfb" at the end
!!      subroutine add_edb_extension(file_header, file_name)
!!                put ".edb" at the end
!!
!!      subroutine add_rtp_extension(file_header, file_name)
!!                put ".rtp" at the end
!!      subroutine add_rtm_extension(file_header, file_name)
!!                put ".rtm" at the end
!!      subroutine add_rlm_extension(file_header, file_name)
!!                put ".rlm" at the end
!!      subroutine add_rj_extension(file_header, file_name)
!!                put ".rj" at the end
!!
!!      subroutine add_btp_extension(file_header, file_name)
!!                put ".btp" at the end
!!      subroutine add_btm_extension(file_header, file_name)
!!                put ".btm" at the end
!!      subroutine add_blm_extension(file_header, file_name)
!!                put ".blm" at the end
!!      subroutine add_brj_extension(file_header, file_name)
!!                put ".brj" at the end
!!
!!      subroutine add_left_label(file_header, file_name)
!!                put "_left" at the end
!!      subroutine add_right_label(file_header, file_name)
!!                put "_right" at the end
!!
!!      subroutine add_index_after_name(int_id, chara_head, chara_name)
!!      subroutine int_to_str(int_val, int_string)
!!      subroutine lint_to_str(lint_val, int_string)
!!      subroutine real_to_str(real_val, real_string)
!!
!!      character(len = kchara) function set_rayleigh_file_name         &
!!     &                               (dir, int_id, postfix)
!!               set file name as "[dir]/[int_id]_[postfix]"
!!@endverbatim
!!
!!@n @param dir_file_name    file name (header) including directory name
!!@n @param int_id           integer to be added at the end of prefix
!!@n @param file_name        output file name
!!@n @param int_val          integer to be tranfered to character
!!@n @param int_string       output character
!
      module set_parallel_file_name
!
      use m_precision
!
      implicit  none
!
!-----------------------------------------------------------------------
!
      contains
!
!-----------------------------------------------------------------------
!
      character(len = kchara) function delete_directory_name            &
     &                               (dir_file_name)
!
      character(len=kchara), intent(in) :: dir_file_name
!
      integer(kind = kint) :: i, len, len_name, idir_flag
      character(len=kchara) :: fmt_txt
!
!
      len_name = len_trim(dir_file_name)
      idir_flag = 0
      do i = len_name, 1, -1
        if(dir_file_name(i:i) .eq. '/') then
          idir_flag = i
          exit
        end if
      end do
      len = len_name - idir_flag
!
      write(fmt_txt,'(a2,i4,a1)') '(a',len,')'
      write(delete_directory_name,fmt_txt)                              &
     &                            dir_file_name(idir_flag+1:len_name)
!
      end function delete_directory_name
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      character(len = kchara) function add_int_suffix                   &
     &                               (int_id, file_header)
!
      integer(kind = kint), intent(in) :: int_id
      character(len=kchara), intent(in) :: file_header
!
      character(len=kchara) :: chara_head
!
!
      write(chara_head,1000) trim(file_header)
      call add_index_after_name(int_id, chara_head, add_int_suffix)
!
 1000 format (a,".")
!
      end function add_int_suffix
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      character(len = kchara) function add_dat_extension(file_header)
!
      character(len=kchara), intent(in) :: file_header
!
       write(add_dat_extension,1011) trim(file_header)
 1011 format (a,".dat")
!
      end function add_dat_extension
!
!-----------------------------------------------------------------------
!
      subroutine add_pvtk_extension(file_header, file_name)
!
      character(len=kchara), intent(in) :: file_header
      character(len=kchara), intent(inout) :: file_name
!
       write(file_name,1011) trim(file_header)
 1011 format (a,".pvtk")
!
      end subroutine add_pvtk_extension
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine add_hdf_extension(file_header, file_name)
!
      character(len=kchara), intent(in) :: file_header
      character(len=kchara), intent(inout) :: file_name
!
       write(file_name,1011) trim(file_header)
 1011 format (a,".h5")
!
      end subroutine add_hdf_extension
!
!-----------------------------------------------------------------------
!
      subroutine add_xdmf_extension(file_header, file_name)
!
      character(len=kchara), intent(in) :: file_header
      character(len=kchara), intent(inout) :: file_name
!
      write(file_name,1011) trim(file_header)
1011 format (a,".xdmf")
!
      end subroutine add_xdmf_extension
!
!-----------------------------------------------------------------------
!
      subroutine add_mesh_suffix(file_header, file_name)
!
      character(len=kchara), intent(in) :: file_header
      character(len=kchara), intent(inout) :: file_name
!
      write(file_name,1011) trim(file_header)
1011 format (a,".mesh")
!
      end subroutine add_mesh_suffix
!
!-----------------------------------------------------------------------
!
      subroutine add_field_suffix(file_header, file_name)
!
      character(len=kchara), intent(in) :: file_header
      character(len=kchara), intent(inout) :: file_name
!
      write(file_name,1011) trim(file_header)
1011 format (a,".field")
!
      end subroutine add_field_suffix
!
!-----------------------------------------------------------------------
!
      subroutine add_xdmf_suffix(file_header, file_name)
!
      character(len=kchara), intent(in) :: file_header
      character(len=kchara), intent(inout) :: file_name
!
      write(file_name,1011) trim(file_header)
1011 format (a,".solution")
!
      end subroutine add_xdmf_suffix
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine add_dx_extension(file_header, file_name)
!
      character(len=kchara), intent(in) :: file_header
      character(len=kchara), intent(inout) :: file_name
!
       write(file_name,1011) trim(file_header)
 1011 format (a,".dx")
!
      end subroutine add_dx_extension
!
!-----------------------------------------------------------------------
!
      subroutine add_node_extension(file_header, file_name)
!
      character(len=kchara), intent(in) :: file_header
      character(len=kchara), intent(inout) :: file_name
!
       write(file_name,1011) trim(file_header)
 1011 format (a,".node.dat")
!
      end subroutine add_node_extension
!
!-----------------------------------------------------------------------
!
      subroutine add_connect_extension(file_header, file_name)
!
      character(len=kchara), intent(in) :: file_header
      character(len=kchara), intent(inout) :: file_name
!
       write(file_name,1011) trim(file_header)
 1011 format (a,".connect.dat")
!
      end subroutine add_connect_extension
!
!-----------------------------------------------------------------------
!
      character(len = kchara) function add_fld_extension(file_header)
!
      character(len=kchara), intent(in) :: file_header
!
       write(add_fld_extension,1011) trim(file_header)
 1011 format (a,".fld")
!
      end function add_fld_extension
!
!-----------------------------------------------------------------------
!
      subroutine add_flb_extension(file_header, file_name)
!
      character(len=kchara), intent(in) :: file_header
      character(len=kchara), intent(inout) :: file_name
!
       write(file_name,1011) trim(file_header)
 1011 format (a,".flb")
!
      end subroutine add_flb_extension
!
!-----------------------------------------------------------------------
!
      subroutine add_fst_extension(file_header, file_name)
!
      character(len=kchara), intent(in) :: file_header
      character(len=kchara), intent(inout) :: file_name
!
       write(file_name,1011) trim(file_header)
 1011 format (a,".fst")
!
      end subroutine add_fst_extension
!
!-----------------------------------------------------------------------
!
      subroutine add_fsb_extension(file_header, file_name)
!
      character(len=kchara), intent(in) :: file_header
      character(len=kchara), intent(inout) :: file_name
!
      write(file_name,1011) trim(file_header)
 1011 format (a,".fsb")
!
end subroutine add_fsb_extension
!
!-----------------------------------------------------------------------
!
      subroutine add_elaps_postfix(file_header, file_name)
!
      character(len=kchara), intent(in) :: file_header
      character(len=kchara), intent(inout) :: file_name
!
       write(file_name,1011) trim(file_header)
 1011 format (a,".elps")
!
      end subroutine add_elaps_postfix
!
!-----------------------------------------------------------------------
!
      character(len = kchara) function add_gzip_extension(file_header)
!
      character(len=kchara), intent(in) :: file_header
!
      write(add_gzip_extension,1011) trim(file_header), ".gz"
 1011 format (a,a3)
!
      end function add_gzip_extension
!
!-----------------------------------------------------------------------
!
      character(len = kchara) function add_null_character(file_header)
!
      character(len=kchara), intent(in) :: file_header
!
      write(add_null_character,1011) trim(file_header), char(0)
 1011 format (a,a1)
!
      end function add_null_character
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine add_ksm_extension(file_header, file_name)
!
      character(len=kchara), intent(in) :: file_header
      character(len=kchara), intent(inout) :: file_name
!
       write(file_name,1011) trim(file_header)
       file_name = trim(file_name)
 1011 format (a,".ksm")
!
      end subroutine add_ksm_extension
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine add_gfm_extension(file_header, file_name)
!
      character(len=kchara), intent(in) :: file_header
      character(len=kchara), intent(inout) :: file_name
!
       write(file_name,1011) trim(file_header)
       file_name = trim(file_name)
 1011 format (a,".gfm")
!
      end subroutine add_gfm_extension
!
!-----------------------------------------------------------------------
!
      subroutine add_gfb_extension(file_header, file_name)
!
      character(len=kchara), intent(in) :: file_header
      character(len=kchara), intent(inout) :: file_name
!
       write(file_name,1011) trim(file_header)
       file_name = trim(file_name)
 1011 format (a,".gfb")
!
      end subroutine add_gfb_extension
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine add_gel_extension(file_header, file_name)
!
      character(len=kchara), intent(in) :: file_header
      character(len=kchara), intent(inout) :: file_name
!
       write(file_name,1011) trim(file_header)
       file_name = trim(file_name)
 1011 format (a,".gel")
!
      end subroutine add_gel_extension
!
!-----------------------------------------------------------------------
!
      subroutine add_gsf_extension(file_header, file_name)
!
      character(len=kchara), intent(in) :: file_header
      character(len=kchara), intent(inout) :: file_name
!
       write(file_name,1011) trim(file_header)
       file_name = trim(file_name)
 1011 format (a,".gsf")
!
      end subroutine add_gsf_extension
!
!-----------------------------------------------------------------------
!
      subroutine add_ged_extension(file_header, file_name)
!
      character(len=kchara), intent(in) :: file_header
      character(len=kchara), intent(inout) :: file_name
!
       write(file_name,1011) trim(file_header)
       file_name = trim(file_name)
 1011 format (a,".ged")
!
      end subroutine add_ged_extension
!
!-----------------------------------------------------------------------
!
      subroutine add_elb_extension(file_header, file_name)
!
      character(len=kchara), intent(in) :: file_header
      character(len=kchara), intent(inout) :: file_name
!
       write(file_name,1011) trim(file_header)
       file_name = trim(file_name)
 1011 format (a,".elb")
!
      end subroutine add_elb_extension
!
!-----------------------------------------------------------------------
!
      subroutine add_sfb_extension(file_header, file_name)
!
      character(len=kchara), intent(in) :: file_header
      character(len=kchara), intent(inout) :: file_name
!
       write(file_name,1011) trim(file_header)
       file_name = trim(file_name)
 1011 format (a,".sfb")
!
      end subroutine add_sfb_extension
!
!-----------------------------------------------------------------------
!
      subroutine add_edb_extension(file_header, file_name)
!
      character(len=kchara), intent(in) :: file_header
      character(len=kchara), intent(inout) :: file_name
!
       write(file_name,1011) trim(file_header)
       file_name = trim(file_name)
 1011 format (a,".edb")
!
      end subroutine add_edb_extension
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine add_rtp_extension(file_header, file_name)
!
      character(len=kchara), intent(in) :: file_header
      character(len=kchara), intent(inout) :: file_name
!
       write(file_name,1011) trim(file_header)
       file_name = trim(file_name)
 1011 format (a,".rtp")
!
      end subroutine add_rtp_extension
!
!-----------------------------------------------------------------------
!
      subroutine add_rtm_extension(file_header, file_name)
!
      character(len=kchara), intent(in) :: file_header
      character(len=kchara), intent(inout) :: file_name
!
       write(file_name,1011) trim(file_header)
       file_name = trim(file_name)
 1011 format (a,".rtm")
!
      end subroutine add_rtm_extension
!
!-----------------------------------------------------------------------
!
      subroutine add_rlm_extension(file_header, file_name)
!
      character(len=kchara), intent(in) :: file_header
      character(len=kchara), intent(inout) :: file_name
!
       write(file_name,1011) trim(file_header)
       file_name = trim(file_name)
 1011 format (a,".rlm")
!
      end subroutine add_rlm_extension
!
!-----------------------------------------------------------------------
!
      subroutine add_rj_extension(file_header, file_name)
!
      character(len=kchara), intent(in) :: file_header
      character(len=kchara), intent(inout) :: file_name
!
       write(file_name,1011) trim(file_header)
       file_name = trim(file_name)
 1011 format (a,".rj")
!
      end subroutine add_rj_extension
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine add_btp_extension(file_header, file_name)
!
      character(len=kchara), intent(in) :: file_header
      character(len=kchara), intent(inout) :: file_name
!
       write(file_name,1011) trim(file_header)
       file_name = trim(file_name)
 1011 format (a,".btp")
!
      end subroutine add_btp_extension
!
!-----------------------------------------------------------------------
!
      subroutine add_btm_extension(file_header, file_name)
!
      character(len=kchara), intent(in) :: file_header
      character(len=kchara), intent(inout) :: file_name
!
       write(file_name,1011) trim(file_header)
       file_name = trim(file_name)
 1011 format (a,".btm")
!
      end subroutine add_btm_extension
!
!-----------------------------------------------------------------------
!
      subroutine add_blm_extension(file_header, file_name)
!
      character(len=kchara), intent(in) :: file_header
      character(len=kchara), intent(inout) :: file_name
!
       write(file_name,1011) trim(file_header)
       file_name = trim(file_name)
 1011 format (a,".blm")
!
      end subroutine add_blm_extension
!
!-----------------------------------------------------------------------
!
      subroutine add_brj_extension(file_header, file_name)
!
      character(len=kchara), intent(in) :: file_header
      character(len=kchara), intent(inout) :: file_name
!
       write(file_name,1011) trim(file_header)
       file_name = trim(file_name)
 1011 format (a,".brj")
!
      end subroutine add_brj_extension
!
!-----------------------------------------------------------------------
!
      subroutine add_left_label(file_header, file_name)
!
      character(len=kchara), intent(in) :: file_header
      character(len=kchara), intent(inout) :: file_name
!
       write(file_name,1011) trim(file_header)
       file_name = trim(file_name)
 1011 format (a,"_left")
!
      end subroutine add_left_label
!
!-----------------------------------------------------------------------
!
      subroutine add_right_label(file_header, file_name)
!
      character(len=kchara), intent(in) :: file_header
      character(len=kchara), intent(inout) :: file_name
!
       write(file_name,1011) trim(file_header)
       file_name = trim(file_name)
 1011 format (a,"_right")
!
      end subroutine add_right_label
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine add_index_after_name(int_id, chara_head, chara_name)
!
      integer(kind = kint), intent(in) :: int_id
      character(len=kchara), intent(in) :: chara_head
      character(len=kchara), intent(inout) :: chara_name
!
      character(len=kchara) :: charaint
!
!
      write(charaint,*) int_id
      write(chara_name,'(a,a)')                                         &
                trim(chara_head), trim(ADJUSTL(charaint))
!
      end subroutine add_index_after_name
!
!-----------------------------------------------------------------------
!
      subroutine int_to_str(int_val, int_string)
!
      integer(kind=kint), intent(in) :: int_val
      character(len=kchara), intent(inout) :: int_string
      character(len=kchara) :: tmp_string
!
!
      write(tmp_string,*) int_val
      write(int_string,'(a)') trim(adjustl(tmp_string))
!
      end subroutine int_to_str
!
!-----------------------------------------------------------------------
!
      subroutine lint_to_str(lint_val, int_string)
!
      integer(kind=kint_gl), intent(in) :: lint_val
      character(len=kchara), intent(inout) :: int_string
      character(len=kchara) :: tmp_string
!
!
      write(tmp_string,*) lint_val
      write(int_string,'(a)') trim(adjustl(tmp_string))
!
      end subroutine lint_to_str
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine real_to_str(real_val, real_string)
!
      real(kind=kreal), intent(in) :: real_val
      character(len=kchara), intent(inout) :: real_string
      character(len=kchara) :: tmp_string
!
!
      write(tmp_string,*) real_val
      write(real_string,'(a)') trim(adjustl(tmp_string))
!
      end subroutine real_to_str
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      character(len = kchara) function set_rayleigh_file_name           &
     &                               (dir, int_id, postfix)
!
      integer(kind = kint), intent(in) :: int_id
      character(len=kchara), intent(in) :: dir, postfix
!
!
      write(set_rayleigh_file_name,1000)                                &
     &                        trim(dir), int_id, trim(postfix)
!
 1000 format(a, '/', i8, '_', a)
!
      end function set_rayleigh_file_name
!
!-----------------------------------------------------------------------
!
      end module set_parallel_file_name
