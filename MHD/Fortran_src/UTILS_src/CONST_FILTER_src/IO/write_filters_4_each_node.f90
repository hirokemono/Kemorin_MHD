!write_filters_4_each_node.f90
!      module write_filters_4_each_node
!
!     Written by H. Matsui on Mar., 2008
!
!!      subroutine write_each_filter_stack_coef                         &
!!     &         (file_name, inod, fil_coef, ierr)
!!      subroutine write_each_no_filter_coef(file_name, inod, ierr)
!!      subroutine write_each_same_filter_coef                          &
!!     &         (file_name, inod, fil_coef, ierr)
!!        type(each_filter_coef), intent(in) :: fil_coef
!!      subroutine read_each_filter_stack_coef                          &
!!     &         (id_file, fil_coef, bbuf, ierr)
!!        type(binary_IO_buffer), intent(inout) :: bbuf
!!        type(each_filter_coef), intent(inout) :: fil_coef
!
      module write_filters_4_each_node
!
      use m_precision
!
      use m_constants
      use calypso_mpi
      use t_filter_coefs
      use t_binary_IO_buffer
      use m_filter_file_names
      use m_field_file_format
      use binary_IO
!
      implicit none
!
      integer(kind = kint), parameter :: id_write_fil =  22
      type(binary_IO_buffer) :: bbuf_flt
      private :: id_write_fil, bbuf_flt
!
! -----------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
      subroutine write_each_filter_stack_coef                           &
     &         (file_name, inod, fil_coef, ierr)
!
      use filter_IO_for_sorting
!
      character(len = kchara), intent(in) :: file_name
      integer(kind = kint), intent(in) :: inod
      type(each_filter_coef), intent(in) :: fil_coef
!
      integer(kind = kint), intent(inout) :: ierr
!
!
      if (ifmt_3d_filter .eq. iflag_ascii) then
        open(org_filter_coef_code, file=file_name,                      &
     &       form='formatted', position='append')
        write(filter_coef_code,'(4i12)')  inod, fil_coef%nnod_4_1nod_w, &
     &                  fil_coef%ilevel_exp_1nod_w
        close(org_filter_coef_code)
        call write_filter_coef_4_each(filter_coef_code, fil_coef)
!
      else if (ifmt_3d_filter .eq. iflag_bin) then
        bbuf_flt%id_binary = id_write_fil
        call open_append_binary_file(file_name, bbuf_flt)
        if(bbuf_flt%ierr_bin .gt. 0) go to 99
        call write_filter_coef_4_each_b(fil_coef, bbuf_flt)
        if(bbuf_flt%ierr_bin .gt. 0) go to 99
!
  99    continue
        call close_binary_file(bbuf_flt)
        ierr = bbuf_flt%ierr_bin
      end if
!
      end subroutine write_each_filter_stack_coef
!
! -----------------------------------------------------------------------
!
      subroutine write_each_no_filter_coef                              &
     &          (file_name, inod, fil_coef, ierr)
!
      character(len = kchara), intent(in) :: file_name
      integer(kind = kint), intent(in) :: inod
      type(each_filter_coef), intent(in) :: fil_coef
      integer(kind = kint), intent(inout) :: ierr
!
!
      if (ifmt_3d_filter .eq. iflag_ascii) then
        open(org_filter_coef_code, file=file_name,                      &
     &       form='formatted', position='append')
        write(org_filter_coef_code,'(4i12)')                            &
     &           inod, fil_coef%nnod_4_1nod_w, izero
        close(org_filter_coef_code)
      else if (ifmt_3d_filter .eq. iflag_bin) then
        bbuf_flt%id_binary = id_write_fil
        call open_append_binary_file(file_name, bbuf_flt)
        if(bbuf_flt%ierr_bin .gt. 0) go to 99
        call write_one_integer_b(fil_coef%nnod_4_1nod_w, bbuf_flt)
        if(bbuf_flt%ierr_bin .ne. 0) go to 99
        call write_one_integer_b(izero, bbuf_flt)
        if(bbuf_flt%ierr_bin .ne. 0) go to 99
!
  99    continue
        call close_binary_file(bbuf_flt)
        ierr = bbuf_flt%ierr_bin
      end if
!
      end subroutine write_each_no_filter_coef
!
! -----------------------------------------------------------------------
!
      subroutine write_each_same_filter_coef                            &
     &         (file_name, inod, fil_coef, ierr)
!
      use mesh_data_IO
      use filter_coefs_file_IO
!
      character(len = kchara), intent(in) :: file_name
      integer(kind = kint), intent(in) :: inod
      type(each_filter_coef), intent(in) :: fil_coef
!
      integer(kind = kint), intent(inout) :: ierr
!
!
      if (ifmt_3d_filter .eq. iflag_ascii) then
        open(org_filter_coef_code, file=file_name,                      &
     &       form='formatted', position='append')
        write(org_filter_coef_code,'(4i12)') inod,                      &
     &          (fil_coef%nnod_4_1nod_w), fil_coef%ilevel_exp_1nod_w
        close(org_filter_coef_code)
      else if (ifmt_3d_filter .eq. iflag_bin) then
        bbuf_flt%id_binary = id_write_fil
        call open_append_binary_file(file_name, bbuf_flt)
        if(bbuf_flt%ierr_bin .gt. 0) go to 99
        call write_one_integer_b(-fil_coef%nnod_4_1nod_w, bbuf_flt)
        if(bbuf_flt%ierr_bin .gt. 0) go to 99
        call write_one_integer_b(fil_coef%ilevel_exp_1nod_w, bbuf_flt)
        if(bbuf_flt%ierr_bin .gt. 0) go to 99
!
  99    continue
        call close_binary_file(bbuf_flt)
        ierr = bbuf_flt%ierr_bin
      end if
!
      end subroutine write_each_same_filter_coef
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine read_each_filter_stack_coef                            &
     &         (id_file, fil_coef, bbuf, ierr)
!
      use t_binary_IO_buffer
      use skip_comment_f
      use filter_IO_for_sorting
!
      integer(kind = kint), intent(in) :: id_file
!
      type(each_filter_coef), intent(inout) :: fil_coef
      type(binary_IO_buffer), intent(inout) :: bbuf
      integer(kind = kint), intent(inout) :: ierr
!
!
      if (ifmt_3d_filter .eq. iflag_ascii) then
        call read_filter_coef_4_each(id_file, fil_coef, ierr)
      else if (ifmt_3d_filter .eq. iflag_bin) then
        call read_filter_coef_4_each_b(bbuf, fil_coef)
      end if
!
      end subroutine read_each_filter_stack_coef
!
! -----------------------------------------------------------------------
!
      end module write_filters_4_each_node
