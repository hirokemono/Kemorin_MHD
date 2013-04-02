!
!      module set_mag_p_sph
!
!      Written by H. Matsui
!
!     subroutine set_mag_p_sph
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
      subroutine s_set_mag_p_sph(ii, i, j)
!
      use m_precision
!
      use m_node_group
      use m_bc_data_list
      use m_bc_data_magne_p
      use m_geometry_parameter
      use m_geometry_data
      use m_schmidt_polynomial
!
      implicit none
!
      integer(kind = kint) :: i, j, k, ii
!
      integer(kind = kint) :: inod
      integer(kind = kint) :: jj,ll,mm
      real ( kind = kreal) :: dph
!
!
      call allocate_schmidt_polynomial
!
!
      jj = int( aint( bc_mag_p_magnitude(j)) )
      ll = int( aint( sqrt(bc_mag_p_magnitude(j)) ))
      mm = jj - ll*(ll+1)
!
      do k=1, bc_istack(i)-bc_istack(i-1)
       ii=ii+1
!
       inod = bc_item(k+bc_istack(i-1))
       ibc_mag_p_id(ii)=inod
!
       dth = colatitude(inod)
       dph = longitude(inod)
       call dschmidt
!
       if (mm.ge.0) then
         bc_mag_p_id_apt(ii) = p(mm,ll) * cos( dph*dble(mm) )
       else
         bc_mag_p_id_apt(ii) = p(mm,ll) * sin( dph*dble(mm) )
       end if
!
        ibc_mag_p(    inod ) = 1
        ibc2_mag_p(    inod ) = 1
!
      end do
!
      call deallocate_schmidt_polynomial
!
      end subroutine s_set_mag_p_sph
!
!-----------------------------------------------------------------------
!
      end module set_mag_p_sph
