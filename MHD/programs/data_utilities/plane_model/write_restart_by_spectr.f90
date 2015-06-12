!
!     module write_restart_by_spectr
!
!      Written by H. Matsui
!      Modified by H. Matsui on June, 2007
!
!      subroutine allocate_rst_by_plane_sp(nnod, ndir)
!      subroutine deallocate_rst_by_plane_sp
!      subroutine plane_nnod_stack_4_IO
!      subroutine s_write_restart_by_spectr(ip, nnod)
!
      module write_restart_by_spectr
!
      use m_precision
      use t_field_data_IO
!
      implicit none
!
!
      real(kind = kreal), allocatable :: rst_from_sp(:,:)
      type(field_IO), save :: pl_fld_IO
!
      private :: pl_fld_IO
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
      subroutine plane_nnod_stack_4_IO
!
      use m_geometry_data_4_merge
!
      integer(kind=kint) :: ip
!
      call alloc_merged_field_stack(num_pe, pl_fld_IO)
!
      pl_fld_IO%istack_numnod_IO(0) = 0
      do ip = 1, num_pe
        pl_fld_IO%istack_numnod_IO(ip)                                  &
     &      = pl_fld_IO%istack_numnod_IO(ip-1)                          &
     &       + subdomain(ip)%node%numnod
      end do
!
      end subroutine plane_nnod_stack_4_IO
!
!  ---------------------------------------------------------------------
!
      subroutine s_write_restart_by_spectr(ip, nnod)
!
      use m_constants
      use m_geometry_data_4_merge
      use field_IO_select
      use set_list_4_FFT
      use set_field_type_to_restart
      use set_restart_data
!
      integer(kind=kint), intent(in) :: ip, nnod
!
      integer(kind=kint) :: id_rank
!
!
      id_rank = ip - 1
!
      pl_fld_IO%nnod_IO = nnod
!
      pl_fld_IO%num_field_IO = merged_fld%num_phys
      pl_fld_IO%ntot_comp_IO = merged_fld%ntot_phys
      call alloc_phys_name_IO(pl_fld_IO)
      call alloc_phys_data_IO(pl_fld_IO)
!
      call simple_copy_fld_name_t_to_rst(merged_fld, pl_fld_IO)
      call simple_copy_fld_dat_to_rst_IO                                &
     &   (nnod, merged_fld%ntot_phys, rst_from_sp,                      &
     &    pl_fld_IO%ntot_comp_IO, pl_fld_IO%nnod_IO, pl_fld_IO%d_IO)
!
      pl_fld_IO%file_prefix = rst_head_plane
      call sel_write_step_FEM_field_file                                &
     &   (num_pe, id_rank, izero, pl_fld_IO)
!
      call dealloc_merged_field_stack(pl_fld_IO)
      call dealloc_phys_name_IO(pl_fld_IO)
      call dealloc_phys_data_IO(pl_fld_IO)
!
      end subroutine s_write_restart_by_spectr
!
!  ---------------------------------------------------------------------
!
      end module write_restart_by_spectr
