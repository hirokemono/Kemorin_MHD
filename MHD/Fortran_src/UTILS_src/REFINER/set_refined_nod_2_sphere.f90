!set_refined_nod_2_sphere.f90
!      module set_refined_nod_2_sphere
!
      module set_refined_nod_2_sphere
!
!     Written by H. Matsui on Oct., 2007
!
      use m_precision
!
      use m_constants
      use m_refined_node_id
!
      implicit none
!
!      subroutine set_x_refine_edge_2_sphere
!      subroutine set_x_refine_surf_2_sphere
!      subroutine set_x_refine_ele_2_sphere
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine set_x_refine_edge_2_sphere
!
!
      integer(kind = kint) :: inod
!
      do inod = 1, ntot_nod_refine_edge
        sph_refine_edge(inod,2)                                         &
     &        = sqrt( x_refine_edge(inod,1)*x_refine_edge(inod,1)       &
     &              + x_refine_edge(inod,2)*x_refine_edge(inod,2)       &
     &              + x_refine_edge(inod,3)*x_refine_edge(inod,3) )
!
        if ( sph_refine_edge(inod,2) .eq. zero) then
          sph_refine_edge(inod,2) = zero
        else
          sph_refine_edge(inod,2) = one / sph_refine_edge(inod,2)
        end if
!
        x_refine_edge(inod,1) =  x_refine_edge(inod,1)                  &
     &                         * sph_refine_edge(inod,1)                &
     &                         * sph_refine_edge(inod,2)
        x_refine_edge(inod,2) =  x_refine_edge(inod,2)                  &
     &                         * sph_refine_edge(inod,1)                &
     &                         * sph_refine_edge(inod,2)
        x_refine_edge(inod,3) =  x_refine_edge(inod,3)                  &
     &                         * sph_refine_edge(inod,1)                &
     &                         * sph_refine_edge(inod,2)
!
      end do
!
      end subroutine set_x_refine_edge_2_sphere
!
!  ---------------------------------------------------------------------
!
      subroutine set_x_refine_surf_2_sphere
!
!
      integer(kind = kint) :: inod
!
!
      do inod = 1, ntot_nod_refine_surf
        sph_refine_surf(inod,2)                                         &
     &        = sqrt( x_refine_surf(inod,1)*x_refine_surf(inod,1)       &
     &              + x_refine_surf(inod,2)*x_refine_surf(inod,2)       &
     &              + x_refine_surf(inod,3)*x_refine_surf(inod,3) )
!
        if ( sph_refine_surf(inod,2) .eq. zero) then
          sph_refine_surf(inod,2) = zero
        else
          sph_refine_surf(inod,2) = one / sph_refine_surf(inod,2)
        end if
!
        x_refine_surf(inod,1) =  x_refine_surf(inod,1)                  &
     &                         * sph_refine_surf(inod,1)                &
     &                         * sph_refine_surf(inod,2)
        x_refine_surf(inod,2) =  x_refine_surf(inod,2)                  &
     &                         * sph_refine_surf(inod,1)                &
     &                         * sph_refine_surf(inod,2)
        x_refine_surf(inod,3) =  x_refine_surf(inod,3)                  &
     &                         * sph_refine_surf(inod,1)                &
     &                         * sph_refine_surf(inod,2)
!
      end do
!
      end subroutine set_x_refine_surf_2_sphere
!
!  ---------------------------------------------------------------------
!
      subroutine set_x_refine_ele_2_sphere
!
!
      integer(kind = kint) :: inod
!
      do inod = 1, ntot_nod_refine_ele
        sph_refine_ele(inod,2)                                          &
     &        = sqrt( x_refine_ele(inod,1)*x_refine_ele(inod,1)         &
     &              + x_refine_ele(inod,2)*x_refine_ele(inod,2)         &
     &              + x_refine_ele(inod,3)*x_refine_ele(inod,3) )
!
        if ( sph_refine_ele(inod,2) .eq. zero) then
          sph_refine_ele(inod,2) = zero
        else
          sph_refine_ele(inod,2) = one / sph_refine_ele(inod,2)
        end if
!
        x_refine_ele(inod,1) =  x_refine_ele(inod,1)                    &
     &                        * sph_refine_ele(inod,1)                  &
     &                        * sph_refine_ele(inod,2)
        x_refine_ele(inod,2) =  x_refine_ele(inod,2)                    &
     &                        * sph_refine_ele(inod,1)                  &
     &                        * sph_refine_ele(inod,2)
        x_refine_ele(inod,3) =  x_refine_ele(inod,3)                    &
     &                        * sph_refine_ele(inod,1)                  &
     &                        * sph_refine_ele(inod,2)
!
      end do
!
      end subroutine set_x_refine_ele_2_sphere
!
!  ---------------------------------------------------------------------
!
      end module set_refined_nod_2_sphere
