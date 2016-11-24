!const_sph_boundary_temp.f90
!      module const_sph_boundary_temp
!
!      programmed by H.Matsui on Sep. 2009
!
!!      subroutine const_sph_temp_bc(node, nod_grp, IO_bc)
!!        type(node_data), intent(in) :: node
!!        type(group_data), intent(in) :: nod_grp
!!        type(IO_boundary), intent(inout) :: IO_bc
!
      module const_sph_boundary_temp
!
      use m_precision
      use m_constants
!
      use t_geometry_data
      use t_group_data
      use t_spherical_harmonics
!
      implicit none
!
      type(sph_1point_type), private :: sph_t
!
      private :: set_sph_temp_bc
!
!-----------------------------------------------------------------------
!
      contains
!
!-----------------------------------------------------------------------
!
      subroutine const_sph_temp_bc(node, nod_grp, IO_bc)
!
      use calypso_mpi
      use m_ctl_params_test_bc_temp
      use t_boundary_field_IO
!
      type(node_data), intent(in) :: node
      type(group_data), intent(in) :: nod_grp
!
      type(IO_boundary), intent(inout) :: IO_bc
!
      integer(kind = kint) :: igrp
!
!
      do igrp = 1, nod_grp%num_grp
        if(nod_grp%grp_name(igrp) .eq. grp_name_nod_bc) then
          igrp_nod_bc = igrp
          exit
        end if
      end do
!
!
      IO_bc%num_group = ione
      call alloc_num_bc_values(IO_bc)
!
      IO_bc%group_type(1) = flag_nod_grp
      IO_bc%group_name(1) = nod_grp%grp_name(igrp_nod_bc)
      IO_bc%field_type(1) = 'temperature'
!
      IO_bc%istack_data(0) = 0
      IO_bc%istack_data(1) = nod_grp%istack_grp(igrp_nod_bc)            &
     &                      - nod_grp%istack_grp(igrp_nod_bc-1)
      IO_bc%ntot_field = IO_bc%istack_data(1)
      call alloc_boundary_values(IO_bc)
!
      call set_sph_temp_bc                                              &
     &   (igrp_nod_bc, l_sph_bc, m_sph_bc, node, nod_grp,               &
     &    IO_bc%ntot_field, IO_bc%id_local_fld, IO_bc%d_field)
!
      call write_boundary_values_file(my_rank, IO_bc)
!
      end subroutine const_sph_temp_bc
!
!-----------------------------------------------------------------------
!
      subroutine set_sph_temp_bc                                        &
     &         (igrp, l, m, node, nod_grp, n_data, inod_bc, bc_temp)
!
      use m_schmidt_polynomial
!
      use spherical_harmonics
!
      type(node_data), intent(in) :: node
      type(group_data), intent(in) :: nod_grp
      integer(kind = kint), intent(in) :: igrp, l, m
      integer(kind = kint), intent(in) :: n_data
      integer(kind = kint), intent(inout) :: inod_bc(n_data)
      real(kind = kreal), intent(inout) :: bc_temp(n_data)
!
      integer(kind = kint) :: j, ist, inum, num, inod
      real(kind = kreal), allocatable:: s(:,:)
!
!
      nth = l
      j = l*(l+1) + m
      call allocate_schmidt_polynomial
!
      call init_sph_indices(nth, sph_t)
      call alloc_spherical_harmonics(sph_t)
!
      ist = nod_grp%istack_grp(igrp-1)
      num = nod_grp%istack_grp(igrp  ) - nod_grp%istack_grp(igrp-1)
      do inum = 1, num
        inod = nod_grp%item_grp(ist+inum)
!
        call dschmidt(node%theta(inod))
        call spheric(jmax_tri_sph, idx, node%phi(inod), sph_t%y_lm)
!
        inod_bc(inum) = inod
        bc_temp(inum) = sph_t%y_lm(j,0)
      end do
!
      call dealloc_spherical_harmonics(sph_t)
      call dealloc_index_4_sph(sph_t)
      call deallocate_schmidt_polynomial
!
      end subroutine set_sph_temp_bc
!
!-----------------------------------------------------------------------
!
      end module const_sph_boundary_temp
