!delete_radial_velocity.f90
!     module delete_radial_velocity
!
!        programmed by H.Matsui on July 2000 (ver 1.1)
!        modified by H.Matsui on Aug., 2007
!
!      subroutine del_radial_velocity(i_field)
!
      module delete_radial_velocity
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
      subroutine del_radial_velocity(i_field)
!
      use m_geometry_data
      use m_node_phys_data
      use m_bc_data_vr0
!
      integer (kind = kint) :: i_field
!
      real (kind = kreal), dimension(3) :: resv
      integer (kind = kint) :: i, inod
!
!
       do i=1, num_bc_vr0_nod
!
        inod = ibc_vr0_id(i)
!
        resv(1) =  xx(inod,1) * a_radius(inod)**2                       &
     &               * ( d_nod(inod,i_field  )*xx(inod,1)               &
     &                 + d_nod(inod,i_field+1)*xx(inod,2)               &
     &                 + d_nod(inod,i_field+2)*xx(inod,3) )
        resv(2) =  xx(inod,2) * a_radius(inod)**2                       &
     &               * ( d_nod(inod,i_field  )*xx(inod,1)               &
     &                 + d_nod(inod,i_field+1)*xx(inod,2)               &
     &                 + d_nod(inod,i_field+2)*xx(inod,3) )
        resv(3) =  xx(inod,3) * a_radius(inod)**2                       &
     &               * ( d_nod(inod,i_field  )*xx(inod,1)               &
     &                + d_nod(inod,i_field+1)*xx(inod,2)                &
     &                + d_nod(inod,i_field+2)*xx(inod,3) )
!
!
        d_nod(inod,i_field  ) = d_nod(inod,i_field  ) - resv(1)
        d_nod(inod,i_field+1) = d_nod(inod,i_field+1) - resv(2)
        d_nod(inod,i_field+2) = d_nod(inod,i_field+2) - resv(3)
!
       end do
!
      end subroutine del_radial_velocity
!
!-----------------------------------------------------------------------
!
      end module delete_radial_velocity
