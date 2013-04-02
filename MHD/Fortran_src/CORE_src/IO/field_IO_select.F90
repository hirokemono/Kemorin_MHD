!field_IO_select.F90
!      module field_IO_select
!
!        programmed by H.Matsui on July, 2006
!        Modified by H.Matsui on May, 2009
!
!      subroutine sel_read_step_FEM_field_file(my_rank, istep_fld)
!      subroutine sel_read_step_SPH_field_file(my_rank, istep_fld)
!
!      subroutine sel_write_step_FEM_field_file(my_rank, istep_fld)
!      subroutine sel_write_step_SPH_field_file(my_rank, istep_fld)
!
!      subroutine check_step_FEM_field_file(my_rank, istep_fld, ierr)
!      subroutine sel_read_alloc_step_FEM_file(my_rank, istep_fld)
!      subroutine sel_read_alloc_step_SPH_file(my_rank, istep_fld)
!
!      subroutine sel_read_alloc_FEM_fld_head(my_rank, istep_fld)
!      subroutine sel_read_alloc_SPH_fld_head(my_rank, istep_fld)
!
      module field_IO_select
!
      use m_precision
!
      use m_file_format_switch
      use m_field_data_IO
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
      subroutine check_step_FEM_field_file(my_rank, istep_fld, ierr)
!
      use set_field_file_names
!
      integer(kind=kint), intent(in) :: my_rank, istep_fld
      integer(kind=kint), intent(inout) :: ierr
      character(len=kchara) :: file_name
!
!
      call set_FEM_fld_file_name(phys_file_head, iflag_field_data_fmt,  &
     &    my_rank, istep_fld, file_name)
!
      write(*,*) 'Check existance of ', trim(file_name)
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
      end subroutine check_step_FEM_field_file
!
!------------------------------------------------------------------
!------------------------------------------------------------------
!
      subroutine sel_write_step_FEM_field_file(my_rank, istep_fld)
!
      use set_field_file_names
!
      integer(kind=kint), intent(in) :: my_rank, istep_fld
      character(len=kchara) :: file_name
!
!
      call set_FEM_fld_file_name(phys_file_head, iflag_field_data_fmt,  &
     &    my_rank, istep_fld, file_name)
!
      call sel_write_step_field_file(file_name, my_rank)
!
      end subroutine sel_write_step_FEM_field_file
!
!------------------------------------------------------------------
!
      subroutine sel_write_step_SPH_field_file(my_rank, istep_fld)
!
      use set_field_file_names
!
      integer(kind=kint), intent(in) :: my_rank, istep_fld
      character(len=kchara) :: file_name
!
!
      call set_SPH_fld_file_name(phys_file_head, iflag_field_data_fmt,  &
     &    my_rank, istep_fld, file_name)
!
      call sel_write_step_field_file(file_name, my_rank)
!
      end subroutine sel_write_step_SPH_field_file
!
!------------------------------------------------------------------
!------------------------------------------------------------------
!
      subroutine sel_read_step_FEM_field_file(my_rank, istep_fld)
!
      use set_field_file_names
!
      integer(kind=kint), intent(in) :: my_rank, istep_fld
      character(len=kchara) :: file_name
!
!
      call set_FEM_fld_file_name(phys_file_head, iflag_field_data_fmt,  &
     &    my_rank, istep_fld, file_name)
!
      call sel_read_step_field_file(file_name, my_rank)
!
      end subroutine sel_read_step_FEM_field_file
!
!------------------------------------------------------------------
!
      subroutine sel_read_step_SPH_field_file(my_rank, istep_fld)
!
      use set_field_file_names
!
      integer(kind=kint), intent(in) :: my_rank, istep_fld
      character(len=kchara) :: file_name
!
!
      call set_SPH_fld_file_name(phys_file_head, iflag_field_data_fmt,  &
     &    my_rank, istep_fld, file_name)
!
      call sel_read_step_field_file(file_name, my_rank)
!
      end subroutine sel_read_step_SPH_field_file
!
!------------------------------------------------------------------
!------------------------------------------------------------------
!
      subroutine sel_read_alloc_step_FEM_file(my_rank, istep_fld)
!
      use set_field_file_names
!
      integer(kind=kint), intent(in) :: my_rank, istep_fld
      character(len=kchara) :: file_name
!
!
      call set_FEM_fld_file_name(phys_file_head, iflag_field_data_fmt,  &
     &    my_rank, istep_fld, file_name)
!
      call sel_read_alloc_step_field_file(file_name, my_rank)
!
      end subroutine sel_read_alloc_step_FEM_file
!
!------------------------------------------------------------------
!
      subroutine sel_read_alloc_step_SPH_file(my_rank, istep_fld)
!
      use set_field_file_names
!
      integer(kind=kint), intent(in) :: my_rank, istep_fld
      character(len=kchara) :: file_name
!
!
      call set_SPH_fld_file_name(phys_file_head, iflag_field_data_fmt,  &
     &    my_rank, istep_fld, file_name)
!
      call sel_read_alloc_step_field_file(file_name, my_rank)
!
      end subroutine sel_read_alloc_step_SPH_file
!
!------------------------------------------------------------------
!
      subroutine sel_read_alloc_FEM_fld_head(my_rank, istep_fld)
!
      use set_field_file_names
!
      integer(kind=kint), intent(in) :: my_rank, istep_fld
      character(len=kchara) :: file_name
!
!
      call set_FEM_fld_file_name(phys_file_head, iflag_field_data_fmt,  &
     &    my_rank, istep_fld, file_name)
!
      call sel_read_alloc_field_head(file_name, my_rank)
!
      end subroutine sel_read_alloc_FEM_fld_head
!
!------------------------------------------------------------------
!
      subroutine sel_read_alloc_SPH_fld_head(my_rank, istep_fld)
!
      use set_field_file_names
!
      integer(kind=kint), intent(in) :: my_rank, istep_fld
      character(len=kchara) :: file_name
!
!
      call set_SPH_fld_file_name(phys_file_head, iflag_field_data_fmt,  &
     &    my_rank, istep_fld, file_name)
!
      call sel_read_alloc_field_head(file_name, my_rank)
!
      end subroutine sel_read_alloc_SPH_fld_head
!
!------------------------------------------------------------------
!------------------------------------------------------------------
!
      subroutine sel_write_step_field_file(file_name, my_rank)
!
      character(len=kchara), intent(in) :: file_name
      integer(kind=kint), intent(in) :: my_rank
!
!
      if(iflag_field_data_fmt .eq. id_binary_file_fmt) then
        call write_step_field_file_b(file_name, my_rank)
!
#ifdef ZLIB_IO
      else if(iflag_field_data_fmt .eq. id_gzip_txt_file_fmt) then
        call write_gz_step_field_file(file_name, my_rank)
#endif
!
      else
        call write_step_field_file(file_name, my_rank)
      end if
!
      end subroutine sel_write_step_field_file
!
!------------------------------------------------------------------
!
      subroutine sel_read_step_field_file(file_name, my_rank)
!
      integer(kind = kint), intent(in) :: my_rank
      character(len=kchara), intent(in) :: file_name
!
!
      if (iflag_field_data_fmt .eq. id_binary_file_fmt) then
        call read_step_field_file_b(file_name, my_rank)
!
#ifdef ZLIB_IO
      else if(iflag_field_data_fmt .eq. id_gzip_txt_file_fmt) then
        call read_gz_step_field_file(file_name, my_rank)
#endif
!
      else
        call read_step_field_file(file_name, my_rank)
      end if
!
      end subroutine sel_read_step_field_file
!
!------------------------------------------------------------------
!
      subroutine sel_read_alloc_step_field_file(file_name, my_rank)
!
      integer(kind = kint), intent(in) :: my_rank
      character(len=kchara), intent(in) :: file_name
!
!
      if (iflag_field_data_fmt .eq. id_binary_file_fmt) then
        call read_and_allocate_step_field_b(file_name, my_rank)
!
#ifdef ZLIB_IO
      else if(iflag_field_data_fmt .eq. id_gzip_txt_file_fmt) then
        call read_alloc_gz_step_field_file(file_name, my_rank)
#endif
!
      else
        call read_and_allocate_step_field(file_name, my_rank)
      end if
!
      end subroutine sel_read_alloc_step_field_file
!
!------------------------------------------------------------------
!
      subroutine sel_read_alloc_field_head(file_name, my_rank)
!
      integer(kind = kint), intent(in) :: my_rank
      character(len=kchara), intent(in) :: file_name
!
!
      if (iflag_field_data_fmt .eq. id_binary_file_fmt) then
        call read_and_allocate_step_head_b(file_name, my_rank)
!
#ifdef ZLIB_IO
      else if(iflag_field_data_fmt .eq. id_gzip_txt_file_fmt) then
        call read_alloc_gz_step_field_head(file_name, my_rank)
#endif
!
      else
        call read_and_allocate_step_head(file_name, my_rank)
      end if
!
      end subroutine sel_read_alloc_field_head
!
!------------------------------------------------------------------
!
      end module field_IO_select
