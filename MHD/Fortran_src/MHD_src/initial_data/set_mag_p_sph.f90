!
!      module set_mag_p_sph
!
!      Written by H. Matsui
!
!      subroutine s_set_mag_p_sph(nod_grp, i, j)
!        type(group_data), intent(in) :: nod_grp
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
      subroutine s_set_mag_p_sph(nod_grp, ii, i, j)
!
      use m_geometry_data
      use m_bc_data_list
      use m_bc_data_magne_p
      use m_schmidt_polynomial
      use t_group_data
      use spherical_harmonics
!
      type(group_data), intent(in) :: nod_grp
      integer(kind = kint), intent(in) :: i, j
      integer(kind = kint), intent(inout) :: ii
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
        ibc_mag_p_id(ii)=inod
!
        call dschmidt(colatitude(inod))
!
        if (mm.ge.0) then
          bc_mag_p_id_apt(ii) = p(mm,ll) * cos(longitude(inod)*dble(mm))
        else
          bc_mag_p_id_apt(ii) = p(mm,ll) * sin(longitude(inod)*dble(mm))
        end if
!
        ibc_mag_p(    inod ) = 1
        ibc2_mag_p(   inod ) = 1
      end do
!
      call deallocate_schmidt_polynomial
!
      end subroutine s_set_mag_p_sph
!
!-----------------------------------------------------------------------
!
      end module set_mag_p_sph
