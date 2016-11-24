!
!      module set_mag_p_sph
!
!      Written by H. Matsui
!
!!      subroutine s_set_mag_p_sph(node, nod_grp, ii, i, j, nod_bc_f)
!!        type(node_data), intent(in) :: node
!!        type(group_data), intent(in) :: nod_grp
!!        type(scaler_fixed_nod_bc_type), intent(inout) :: nod_bc_f
!
      module set_mag_p_sph
!
      use m_precision
!
      use t_geometry_data
      use t_group_data
      use t_nodal_bc_data
      use t_schmidt_polynomial
!
      implicit none
!
      integer(kind = kint), parameter, private :: ltr_ini = 10
      type(legendre_polynomials), private :: leg_p
!
!-----------------------------------------------------------------------
!
      contains
!
!-----------------------------------------------------------------------
!
      subroutine s_set_mag_p_sph(node, nod_grp, ii, i, j, nod_bc_f)
!
      use m_bc_data_list
      use spherical_harmonics
!
      type(node_data), intent(in) :: node
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
      call alloc_schmidt_polynomial(ltr_ini, leg_p)
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
        call dschmidt(node%theta(inod), leg_p)
!
        if (mm.ge.0) then
          nod_bc_f%bc_apt(ii)                                           &
     &          = leg_p%p(mm,ll) * cos(node%phi(inod)*dble(mm))
        else
          nod_bc_f%bc_apt(ii)                                           &
     &          = leg_p%p(mm,ll) * sin(node%phi(inod)*dble(mm))
        end if
!
        nod_bc_f%ibc(    inod ) = 1
        nod_bc_f%ibc2(   inod ) = 1
      end do
!
      call dealloc_schmidt_polynomial(leg_p)
!
      end subroutine s_set_mag_p_sph
!
!-----------------------------------------------------------------------
!
      end module set_mag_p_sph
