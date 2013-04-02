!
!     module write_restart_by_spectr
!
!      Written by H. Matsui
!      Modified by H. Matsui on June, 2007
!
!      subroutine allocate_rst_by_plane_sp(nnod, ndir)
!      subroutine deallocate_rst_by_plane_sp
!      subroutine s_write_restart_by_spectr(ip, nnod)
!
      module write_restart_by_spectr
!
      use m_precision
!
      implicit none
!
!
      real(kind = kreal), allocatable :: rst_from_sp(:,:)
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine allocate_rst_by_plane_sp(nnod, ndir)
!
      integer(kind = kint), intent(in) :: nnod, ndir
!
!
      allocate(rst_from_sp(nnod,ndir))
      rst_from_sp = 0.0e0
!
      end subroutine allocate_rst_by_plane_sp
!
!  ---------------------------------------------------------------------
!
      subroutine deallocate_rst_by_plane_sp
!
      deallocate(rst_from_sp)
!
      end subroutine deallocate_rst_by_plane_sp
!
!  ---------------------------------------------------------------------
!
      subroutine s_write_restart_by_spectr(ip, nnod)
!
      use m_constants
      use m_geometry_data_4_merge
      use m_field_data_IO
      use field_IO_select
      use set_list_4_FFT
!
      integer(kind=kint), intent(in) :: ip, nnod
!
      integer(kind=kint) :: my_rank
!
!
      my_rank = ip - 1
!
      numgrid_phys_IO = nnod
!
      num_phys_data_IO = merged_fld%num_phys
      ntot_phys_data_IO = merged_fld%ntot_phys
      call allocate_phys_data_name_IO
      call allocate_phys_data_IO
!
      phys_data_name_IO(1:num_phys_data_IO)                             &
     &             = merged_fld%phys_name(1:num_phys_data_IO)
      num_phys_comp_IO(1:num_phys_data_IO)                              &
     &             = merged_fld%num_component(1:num_phys_data_IO)
      istack_phys_comp_IO(0:num_phys_data_IO)                           &
     &             = merged_fld%istack_component(0:num_phys_data_IO)
      phys_data_IO(1:nnod,1:ntot_phys_data_IO)                          &
     &             = rst_from_sp(1:nnod,1:ntot_phys_data_IO)
!
      phys_file_head = rst_head_plane
      call sel_write_step_FEM_field_file(my_rank, izero)
!
      call deallocate_phys_data_name_IO
      call deallocate_phys_data_IO
!
      end subroutine s_write_restart_by_spectr
!
!  ---------------------------------------------------------------------
!
      end module write_restart_by_spectr
