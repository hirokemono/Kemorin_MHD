!const_sph_boundary_temp.f90
!      module const_sph_boundary_temp
!
!      programmed by H.Matsui on Sep. 2009
!
!      subroutine const_sph_temp_bc(igrp, l, m)
!
      module const_sph_boundary_temp
!
      use m_precision
      use m_constants
!
      implicit none
!
      private :: set_sph_temp_bc
!
!-----------------------------------------------------------------------
!
      contains
!
!-----------------------------------------------------------------------
!
      subroutine const_sph_temp_bc(nod_grp)
!
      use calypso_mpi
      use t_group_data
      use m_ctl_params_test_bc_temp
      use m_boundary_field_IO
      use boundary_field_file_IO
!
      type(group_data), intent(in) :: nod_grp
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
      num_bc_group_IO = ione
      call allocate_num_bc_values
!
      bc_group_type_IO(1) = flag_nod_grp
      bc_data_group_IO(1) = nod_grp%grp_name(igrp_nod_bc)
      bc_field_type_IO(1) = 'temperature'
!
      istack_bc_data_IO(0) = 0
      istack_bc_data_IO(1) = nod_grp%istack_grp(igrp_nod_bc)            &
     &                      - nod_grp%istack_grp(igrp_nod_bc-1)
      ntot_boundary_field_IO = istack_bc_data_IO(1)
      call allocate_boundary_values
!
      call set_sph_temp_bc(igrp_nod_bc, l_sph_bc, m_sph_bc, nod_grp,    &
     &    ntot_boundary_field_IO, id_local_bc_fld_IO(1),                &
     &    boundary_field_IO(1))
!
      call write_boundary_values_file(my_rank)
!
      end subroutine const_sph_temp_bc
!
!-----------------------------------------------------------------------
!
      subroutine set_sph_temp_bc                                        &
     &         (igrp, l, m, nod_grp, n_data, inod_bc, bc_temp)
!
      use m_geometry_data
      use t_group_data
!
      use m_schmidt_polynomial
      use m_spherical_harmonics
!
      use spherical_harmonics
!
      type(group_data), intent(in) :: nod_grp
      integer(kind = kint), intent(in) :: igrp, l, m
      integer(kind = kint), intent(in) :: n_data
      integer(kind = kint), intent(inout) :: inod_bc(n_data)
      real(kind = kreal), intent(inout) :: bc_temp(n_data)
!
      integer(kind = kint) :: j, ist, inum, num, inod
!
      nth = l
      j = l*(l+1) + m
      call allocate_schmidt_polynomial
      call allocate_spherical_harmonics(nth)
!
      ist = nod_grp%istack_grp(igrp-1)
      num = nod_grp%istack_grp(igrp  ) - nod_grp%istack_grp(igrp-1)
      do inum = 1, num
        inod = nod_grp%item_grp(ist+inum)
!
        call dschmidt(colatitude(inod))
        call spheric(longitude(inod))
!
        inod_bc(inum) = inod
        bc_temp(inum) = s(j,0)
      end do
!
      call deallocate_schmidt_polynomial
      call deallocate_spherical_harmonics
!
      end subroutine set_sph_temp_bc
!
!-----------------------------------------------------------------------
!
      end module const_sph_boundary_temp
