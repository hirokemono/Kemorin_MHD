!> @file  field_type_IO_select.f90
!!      module field_type_IO_select
!!
!! @author  H. Matsui
!! @date Programmed in Nov., 2008
!
!> @brief read restart file
!!
!!@verbatim
!!      subroutine sel_write_step_SPH_fld_t_file                        &
!!     &         (my_rank, istep_fld, fld_IO)
!!      subroutine sel_read_step_SPH_fld_t_file                         &
!!     &         (my_rank, istep_fld, fld_IO)
!!
!!      subroutine check_step_sph_t_file                                &
!!     &         (my_rank, istep_fld, fld_IO, ierr)
!!      subroutine sel_read_alloc_step_SPH_fld_t                        &
!!     &         (my_rank, istep_fld, fld_IO)
!!
!!      subroutine sel_read_alloc_SPH_fld_head_t                        &
!!     &         (my_rank, istep_fld, fld_IO)
!!@endverbatim
!
      module field_type_IO_select
!
      use m_precision
!
      use m_file_format_switch
      use t_field_data_IO
      use set_field_file_names
      use field_type_IO
      use field_type_IO_b
!
#ifdef ZLIB_IO
      use gz_field_type_file_IO
#endif
!
      implicit none
!
!------------------------------------------------------------------
!
      contains
!
!------------------------------------------------------------------
!
      subroutine sel_write_step_SPH_fld_t_file                          &
     &         (my_rank, istep_fld, fld_IO)
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
      if(fld_IO%iflag_file_fmt .eq. id_binary_file_fmt) then
        call write_step_fld_type_file_b(file_name, my_rank, fld_IO)
!
#ifdef ZLIB_IO
      else if(fld_IO%iflag_file_fmt .eq. id_gzip_txt_file_fmt) then
        call write_step_fld_type_file_gz(file_name, my_rank, fld_IO)
#endif
!
      else
        call write_step_field_type_file(file_name, my_rank, fld_IO)
      end if
!
      end subroutine sel_write_step_SPH_fld_t_file
!
!------------------------------------------------------------------
!------------------------------------------------------------------
!
      subroutine sel_read_step_SPH_fld_t_file                           &
     &         (my_rank, istep_fld, fld_IO)
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
      if (fld_IO%iflag_file_fmt .eq. id_binary_file_fmt) then
        call read_step_fld_type_file_b(file_name, my_rank, fld_IO)
!
#ifdef ZLIB_IO
      else if(fld_IO%iflag_file_fmt .eq. id_gzip_txt_file_fmt) then
        call read_step_fld_type_file_gz(file_name, my_rank, fld_IO)
#endif
!
      else
        call read_step_field_type_file(file_name, my_rank, fld_IO)
      end if
!
      end subroutine sel_read_step_SPH_fld_t_file
!
!------------------------------------------------------------------
!------------------------------------------------------------------
!
      subroutine check_step_sph_t_file                                  &
     &         (my_rank, istep_fld, fld_IO, ierr)
!
      integer(kind=kint), intent(in) :: my_rank, istep_fld
      type(field_IO), intent(inout) :: fld_IO
      integer(kind=kint), intent(inout) :: ierr
!
      character(len=kchara) :: file_name
!
!
      call set_SPH_fld_file_name(fld_IO%file_prefix,                    &
     &    fld_IO%iflag_file_fmt, my_rank, istep_fld, file_name)
!
      open (id_phys_file, file=file_name, status='old', err=99)
      close(id_phys_file)
!
      ierr = 0
      return
!
  99  continue
      ierr = 1
      return
!
      end subroutine check_step_sph_t_file
!
!------------------------------------------------------------------
!
      subroutine sel_read_alloc_step_SPH_fld_t                          &
     &         (my_rank, istep_fld, fld_IO)
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
      if (fld_IO%iflag_file_fmt .eq. id_binary_file_fmt) then
        call read_and_alloc_step_fld_t_b(file_name, my_rank, fld_IO)
!
#ifdef ZLIB_IO
      else if(fld_IO%iflag_file_fmt .eq. id_gzip_txt_file_fmt) then
        call read_and_alloc_step_fld_t_gz(file_name, my_rank, fld_IO)
#endif
!
      else
        call read_and_alloc_step_field_t(file_name, my_rank, fld_IO)
      end if
!
      end subroutine sel_read_alloc_step_SPH_fld_t
!
!------------------------------------------------------------------
!------------------------------------------------------------------
!
      subroutine sel_read_alloc_SPH_fld_head_t                          &
     &         (my_rank, istep_fld, fld_IO)
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
      if (fld_IO%iflag_file_fmt .eq. id_binary_file_fmt) then
        call read_alloc_step_fld_t_head_b(file_name, my_rank, fld_IO)
!
#ifdef ZLIB_IO
      else if(fld_IO%iflag_file_fmt .eq. id_gzip_txt_file_fmt) then
        call read_alloc_step_fld_t_head_gz(file_name, my_rank, fld_IO)
#endif
!
      else
        call read_alloc_step_fld_t_head(file_name, my_rank, fld_IO)
      end if
!
      end subroutine sel_read_alloc_SPH_fld_head_t
!
!------------------------------------------------------------------
!
      end module field_type_IO_select
