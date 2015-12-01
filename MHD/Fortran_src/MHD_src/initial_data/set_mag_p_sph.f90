!
!      module set_mag_p_sph
!
!      Written by H. Matsui
!
!!      subroutine s_set_mag_p_sph(nod_grp, ii, i, j, nod_bc_f)
!!        type(group_data), intent(in) :: nod_grp
!!        type(scaler_fixed_nod_bc_type), intent(inout) :: nod_bc_f
!
      module set_mag_p_sph
!
      use m_precision
!
      implicit none
!
!-----------------------------------------------------------------------
!
      contains
!
!-----------------------------------------------------------------------
!
      subroutine s_set_mag_p_sph(nod_grp, ii, i, j, nod_bc_f)
!
      use m_geometry_data
      use m_bc_data_list
      use m_schmidt_polynomial
      use t_group_data
      use t_nodal_bc_data
      use spherical_harmonics
!
      type(group_data), intent(in) :: nod_grp
      integer(kind = kint), intent(in) :: i, j
!
      integer(kind = kint), intent(inout) :: ii
      type(scaler_fixed_nod_bc_type), intent(inout) :: nod_bc_f
!
      integer(kind = kint) :: k, jj
      integer(kind = kint) :: inod
      integer(kind = kint) :: ll, mm
!
!
      call allocate_schmidt_polynomial
!
!
      jj = int(e_potential_nod%bc_magnitude(j))
      call get_dgree_order_by_full_j(jj, ll, mm)
!
      do k=1, nod_grp%istack_grp(i)-nod_grp%istack_grp(i-1)
        ii=ii+1
!
        inod = nod_grp%item_grp(k+nod_grp%istack_grp(i-1))
        nod_bc_f%ibc_id(ii) = inod
!
        call dschmidt(node1%theta(inod))
!
        if (mm.ge.0) then
          nod_bc_f%bc_apt(ii)                                           &
     &          = p(mm,ll) * cos(node1%phi(inod)*dble(mm))
        else
          nod_bc_f%bc_apt(ii)                                           &
     &          = p(mm,ll) * sin(node1%phi(inod)*dble(mm))
        end if
!
        nod_bc_f%ibc(    inod ) = 1
        nod_bc_f%ibc2(   inod ) = 1
      end do
!
      call deallocate_schmidt_polynomial
!
      end subroutine s_set_mag_p_sph
!
!-----------------------------------------------------------------------
!
      end module set_mag_p_sph
