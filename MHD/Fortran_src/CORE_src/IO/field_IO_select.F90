!> @file  field_IO_select.F90
!!      module field_IO_select
!!
!!@author  H. Matsui
!!@date Programmed in July, 2006
!!@n    Modified in May, 2009
!!@n    Modified in June, 2015
!
!> @brief read and write restart file
!!
!!@verbatim
!!      subroutine check_step_FEM_field_file                            &
!!     &         (my_rank, istep_fld, fld_IO, ierr)
!!
!!      subroutine sel_write_step_FEM_field_file                        &
!!     &         (my_rank, istep_fld, fld_IO)
!!      subroutine sel_write_step_SPH_field_file                        &
!!     &         (my_rank, istep_fld, fld_IO)
!!
!!      subroutine sel_read_step_FEM_field_file                         &
!!     &         (my_rank, istep_fld, fld_IO)
!!      subroutine sel_read_step_SPH_field_file                         &
!!     &         (my_rank, istep_fld, fld_IO)
!!
!!      subroutine sel_read_alloc_step_FEM_file                         &
!!     &         (my_rank, istep_fld, fld_IO)
!!      subroutine sel_read_alloc_step_SPH_file                         &
!!     &         (my_rank, istep_fld, fld_IO)
!!
!!      subroutine sel_read_alloc_FEM_fld_head                          &
!!     &         (my_rank, istep_fld, fld_IO)
!!      subroutine sel_read_alloc_SPH_fld_head                          &
!!     &         (my_rank, istep_fld, fld_IO)
!!@endverbatim
!
      module field_IO_select
!
      use m_precision
!
      use m_file_format_switch
      use t_field_data_IO
      use field_file_IO
      use field_file_IO_b
!
#ifdef ZLIB_IO
      use gz_field_file_IO
#endif
!
      implicit none
!
      private :: sel_write_step_field_file, sel_read_step_field_file
      private :: sel_read_alloc_step_field_file
      private :: sel_read_alloc_field_head
!
!------------------------------------------------------------------
!
      contains
!
!------------------------------------------------------------------
!
      subroutine check_step_FEM_field_file                              &
     &         (my_rank, istep_fld, fld_IO, ierr)
!
      use set_field_file_names
      use delete_data_files
!
      integer(kind=kint), intent(in) :: my_rank, istep_fld
      type(field_IO), intent(in) :: fld_IO
      integer(kind=kint), intent(inout) :: ierr
      character(len=kchara) :: file_name
!
!
      call set_FEM_fld_file_name(fld_IO%file_prefix,                    &
     &    fld_IO%iflag_file_fmt, my_rank, istep_fld, file_name)
!
      ierr = check_file_exist(file_name)
!
      end subroutine check_step_FEM_field_file
!
!------------------------------------------------------------------
!------------------------------------------------------------------
!
      subroutine sel_write_step_FEM_field_file                          &
     &         (my_rank, istep_fld, fld_IO)
!
      use set_field_file_names
!
      integer(kind=kint), intent(in) :: my_rank, istep_fld
      type(field_IO), intent(in) :: fld_IO
      character(len=kchara) :: file_name
!
!
      call set_FEM_fld_file_name(fld_IO%file_prefix,                    &
     &    fld_IO%iflag_file_fmt, my_rank, istep_fld, file_name)
!
      call sel_write_step_field_file(file_name, my_rank, fld_IO)
!
      end subroutine sel_write_step_FEM_field_file
!
!------------------------------------------------------------------
!
      subroutine sel_write_step_SPH_field_file                          &
     &         (my_rank, istep_fld, fld_IO)
!
      use set_field_file_names
!
      integer(kind=kint), intent(in) :: my_rank, istep_fld
      type(field_IO), intent(in) :: fld_IO
      character(len=kchara) :: file_name
!
!
      call set_SPH_fld_file_name(fld_IO%file_prefix,                    &
     &    fld_IO%iflag_file_fmt, my_rank, istep_fld, file_name)
!
      call sel_write_step_field_file(file_name, my_rank, fld_IO)
!
      end subroutine sel_write_step_SPH_field_file
!
!------------------------------------------------------------------
!------------------------------------------------------------------
!
      subroutine sel_read_step_FEM_field_file                           &
     &         (my_rank, istep_fld, fld_IO)
!
      use set_field_file_names
!
      integer(kind=kint), intent(in) :: my_rank, istep_fld
      type(field_IO), intent(inout) :: fld_IO
!
      character(len=kchara) :: file_name
!
!
      call set_FEM_fld_file_name(fld_IO%file_prefix,                    &
     &    fld_IO%iflag_file_fmt, my_rank, istep_fld, file_name)
!
      call sel_read_step_field_file(file_name, my_rank, fld_IO)
!
      end subroutine sel_read_step_FEM_field_file
!
!------------------------------------------------------------------
!
      subroutine sel_read_step_SPH_field_file                           &
     &         (my_rank, istep_fld, fld_IO)
!
      use set_field_file_names
!
      integer(kind=kint), intent(in) :: my_rank, istep_fld
      type(field_IO), intent(inout) :: fld_IO
!
      character(len=kchara) :: file_name
!
!
      call set_SPH_fld_file_name(fld_IO%file_prefix,                    &
     &    fld_IO%iflag_file_fmt, my_rank, istep_fld, file_name)
!
      call sel_read_step_field_file(file_name, my_rank, fld_IO)
!
      end subroutine sel_read_step_SPH_field_file
!
!------------------------------------------------------------------
!------------------------------------------------------------------
!
      subroutine sel_read_alloc_step_FEM_file                           &
     &         (my_rank, istep_fld, fld_IO)
!
      use set_field_file_names
!
      integer(kind=kint), intent(in) :: my_rank, istep_fld
      type(field_IO), intent(inout) :: fld_IO
!
      character(len=kchara) :: file_name
!
!
      call set_FEM_fld_file_name(fld_IO%file_prefix,                    &
     &    fld_IO%iflag_file_fmt, my_rank, istep_fld, file_name)
!
      call sel_read_alloc_step_field_file(file_name, my_rank, fld_IO)
!
      end subroutine sel_read_alloc_step_FEM_file
!
!------------------------------------------------------------------
!
      subroutine sel_read_alloc_step_SPH_file                           &
     &         (my_rank, istep_fld, fld_IO)
!
      use set_field_file_names
!
      integer(kind=kint), intent(in) :: my_rank, istep_fld
      type(field_IO), intent(inout) :: fld_IO
!
      character(len=kchara) :: file_name
!
!
      call set_SPH_fld_file_name(fld_IO%file_prefix,                    &
     &    fld_IO%iflag_file_fmt, my_rank, istep_fld, file_name)
!
      call sel_read_alloc_step_field_file(file_name, my_rank, fld_IO)
!
      end subroutine sel_read_alloc_step_SPH_file
!
!------------------------------------------------------------------
!
      subroutine sel_read_alloc_FEM_fld_head                            &
     &         (my_rank, istep_fld, fld_IO)
!
      use set_field_file_names
!
      integer(kind=kint), intent(in) :: my_rank, istep_fld
      type(field_IO), intent(inout) :: fld_IO
!
      character(len=kchara) :: file_name
!
!
      call set_FEM_fld_file_name(fld_IO%file_prefix,                    &
     &    fld_IO%iflag_file_fmt, my_rank, istep_fld, file_name)
!
      call sel_read_alloc_field_head(file_name, my_rank, fld_IO)
!
      end subroutine sel_read_alloc_FEM_fld_head
!
!------------------------------------------------------------------
!
      subroutine sel_read_alloc_SPH_fld_head                            &
     &         (my_rank, istep_fld, fld_IO)
!
      use set_field_file_names
!
      integer(kind=kint), intent(in) :: my_rank, istep_fld
      type(field_IO), intent(inout) :: fld_IO
!
      character(len=kchara) :: file_name
!
!
      call set_SPH_fld_file_name(fld_IO%file_prefix,                    &
     &    fld_IO%iflag_file_fmt, my_rank, istep_fld, file_name)
!
      call sel_read_alloc_field_head(file_name, my_rank, fld_IO)
!
      end subroutine sel_read_alloc_SPH_fld_head
!
!------------------------------------------------------------------
!------------------------------------------------------------------
!
      subroutine sel_write_step_field_file(file_name, my_rank, fld_IO)
!
      character(len=kchara), intent(in) :: file_name
      integer(kind=kint), intent(in) :: my_rank
      type(field_IO), intent(in) :: fld_IO
!
!
      if(fld_IO%iflag_file_fmt .eq. id_binary_file_fmt) then
        call write_step_field_file_b(file_name, my_rank, fld_IO)
!
#ifdef ZLIB_IO
      else if(fld_IO%iflag_file_fmt .eq. id_gzip_txt_file_fmt) then
        call write_gz_step_field_file(file_name, my_rank, fld_IO)
#endif
!
      else
        call write_step_field_file(file_name, my_rank, fld_IO)
      end if
!
      end subroutine sel_write_step_field_file
!
!------------------------------------------------------------------
!
      subroutine sel_read_step_field_file(file_name, my_rank, fld_IO)
!
      integer(kind = kint), intent(in) :: my_rank
      character(len=kchara), intent(in) :: file_name
      type(field_IO), intent(inout) :: fld_IO
!
!
      if (fld_IO%iflag_file_fmt .eq. id_binary_file_fmt) then
        call read_step_field_file_b(file_name, my_rank, fld_IO)
!
#ifdef ZLIB_IO
      else if(fld_IO%iflag_file_fmt .eq. id_gzip_txt_file_fmt) then
        call read_gz_step_field_file(file_name, my_rank, fld_IO)
#endif
!
      else
        call read_step_field_file(file_name, my_rank, fld_IO)
      end if
!
      end subroutine sel_read_step_field_file
!
!------------------------------------------------------------------
!
      subroutine sel_read_alloc_step_field_file                         &
     &         (file_name, my_rank, fld_IO)
!
      integer(kind = kint), intent(in) :: my_rank
      character(len=kchara), intent(in) :: file_name
      type(field_IO), intent(inout) :: fld_IO
!
!
      if (fld_IO%iflag_file_fmt .eq. id_binary_file_fmt) then
        call read_and_allocate_step_field_b(file_name, my_rank, fld_IO)
!
#ifdef ZLIB_IO
      else if(fld_IO%iflag_file_fmt .eq. id_gzip_txt_file_fmt) then
        call read_alloc_gz_step_field_file(file_name, my_rank, fld_IO)
#endif
!
      else
        call read_and_alloc_step_field(file_name, my_rank, fld_IO)
      end if
!
      end subroutine sel_read_alloc_step_field_file
!
!------------------------------------------------------------------
!
      subroutine sel_read_alloc_field_head(file_name, my_rank, fld_IO)
!
      integer(kind = kint), intent(in) :: my_rank
      character(len=kchara), intent(in) :: file_name
      type(field_IO), intent(inout) :: fld_IO
!
!
      if (fld_IO%iflag_file_fmt .eq. id_binary_file_fmt) then
        call read_and_allocate_step_head_b(file_name, my_rank, fld_IO)
!
#ifdef ZLIB_IO
      else if(fld_IO%iflag_file_fmt .eq. id_gzip_txt_file_fmt) then
        call read_alloc_gz_step_field_head(file_name, my_rank, fld_IO)
#endif
!
      else
        call read_and_allocate_step_head(file_name, my_rank, fld_IO)
      end if
!
      end subroutine sel_read_alloc_field_head
!
!------------------------------------------------------------------
!
      end module field_IO_select
