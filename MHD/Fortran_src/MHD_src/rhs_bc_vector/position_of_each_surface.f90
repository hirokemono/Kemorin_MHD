!position_of_each_surface.f90
!      module position_of_each_surface
!
!      Written by H. Matsui on Sep., 2005
!      Modified by H. Matsui on Jan., 2009
!
!      subroutine position_2_each_surface
!        Input:  xx, Output:  xe_sf
!      subroutine delta_x_2_each_surface
!        Output:  dxe_sf
!
!      subroutine position_2_each_surf_grp(np_smp, numnod, numele,      &
!     &          nnod_4_ele, nnod_4_surf, node_on_sf, ie, xx, a_radius, &
!     &          num_surf, num_surf_bc, surf_istack, surf_item,         &
!     &          num_surf_smp, isurf_grp_smp_stack, xe_sf)
!
!      subroutine delta_x_2_each_surf_grp(np_smp, numnod, numele,       &
!     &          nnod_4_ele, nnod_4_surf, node_on_sf, node_on_sf_n,     &
!     &          ie, xx, num_surf, num_surf_bc,                         &
!     &          surf_istack, surf_item, num_surf_smp,                  &
!     &          isurf_grp_smp_stack, dxe_sf)
!
      module position_of_each_surface
!
      use m_precision
!
      implicit none
!
! ----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
      subroutine position_2_each_surface
!
      use m_geometry_parameter
      use m_machine_parameter
      use m_geometry_data
      use m_surface_group
      use m_int_surface_data
!
      call position_2_each_surf_grp(np_smp, numnod, numele, nnod_4_ele, &
     &    nnod_4_surf, node_on_sf, ie, xx, a_radius,                    &
     &    sf_grp1%num_grp, num_surf_bc, surf_istack, surf_item,         &
     &    num_surf_smp, isurf_grp_smp_stack, xe_sf)
!
      end subroutine position_2_each_surface
!
! ----------------------------------------------------------------------
!
      subroutine delta_x_2_each_surface
!
      use m_geometry_parameter
      use m_machine_parameter
      use m_geometry_data
      use m_surface_group
      use m_int_surface_data
!
      call delta_x_2_each_surf_grp(np_smp, numnod, numele, nnod_4_ele,  &
     &    nnod_4_surf, node_on_sf, node_on_sf_n, ie, xx,                &
     &    sf_grp1%num_grp, num_surf_bc, surf_istack, surf_item,         &
     &    num_surf_smp, isurf_grp_smp_stack, dxe_sf)
!
      end subroutine delta_x_2_each_surface
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      subroutine position_2_each_surf_grp(np_smp, numnod, numele,       &
     &          nnod_4_ele, nnod_4_surf, node_on_sf, ie, xx, a_radius,  &
     &          num_surf, num_surf_bc, surf_istack, surf_item,          &
     &          num_surf_smp, isurf_grp_smp_stack, xe_sf)
!
      use m_geometry_constants
!
      integer(kind = kint), intent(in) :: numnod, nnod_4_surf
      integer(kind = kint), intent(in) :: numele, nnod_4_ele
      integer(kind = kint), intent(in) :: ie(numele,nnod_4_ele)
      integer(kind = kint), intent(in)                                  &
     &                  :: node_on_sf(nnod_4_surf,nsurf_4_ele)
      real(kind = kreal), intent(in) :: xx(numnod,3)
      real(kind = kreal), intent(in) :: a_radius(numnod)
!
      integer(kind = kint), intent(in) :: np_smp, num_surf_smp
      integer(kind = kint), intent(in)                                  &
     &            :: isurf_grp_smp_stack(0:num_surf_smp)
      integer(kind = kint), intent(in) :: num_surf, num_surf_bc
      integer(kind = kint), intent(in) :: surf_istack(0:num_surf)
      integer(kind = kint), intent(in) :: surf_item(2,num_surf_bc)
!
      real(kind=kreal), intent(inout)                                   &
     &      :: xe_sf(num_surf_bc,4,nnod_4_surf)
!
      integer (kind = kint) :: igrp, iproc, id_sf
      integer (kind = kint) :: ist, ied, inum, iele, isf
      integer (kind = kint) :: k2, kk2, inod, nsf
!
!
      do igrp = 1, num_surf
!
        nsf = surf_istack(igrp) - surf_istack(igrp-1)
        if (nsf.gt.0) then
!
!$omp parallel do &
!$omp& private(id_sf,ist,ied,inum,iele,isf,kk2,inod)
          do iproc = 1, np_smp
            id_sf = np_smp*(igrp-1) + iproc
            ist = isurf_grp_smp_stack(id_sf-1)+1
            ied = isurf_grp_smp_stack(id_sf)
!
            do k2 = 1, nnod_4_surf
!cdir nodep
              do inum = ist, ied
!
                iele = surf_item(1,inum)
                isf =  surf_item(2,inum)
                kk2 =  node_on_sf(k2,isf)
                inod = ie(iele,kk2)
!
                xe_sf(inum,1,k2) = xx(inod,1)
                xe_sf(inum,2,k2) = xx(inod,2)
                xe_sf(inum,3,k2) = xx(inod,3)
                xe_sf(inum,4,k2) = a_radius(inod)
!
              end do
            end do
          end do
!$omp end parallel do
!
        end if
      end do
!
      end subroutine position_2_each_surf_grp
!
! ----------------------------------------------------------------------
!
      subroutine delta_x_2_each_surf_grp(np_smp, numnod, numele,        &
     &          nnod_4_ele, nnod_4_surf, node_on_sf, node_on_sf_n,      &
     &          ie, xx, num_surf, num_surf_bc,                          &
     &          surf_istack, surf_item, num_surf_smp,                   &
     &          isurf_grp_smp_stack, dxe_sf)
!
      use m_constants
      use m_geometry_constants
!
      integer(kind = kint), intent(in) :: numnod, nnod_4_surf
      integer(kind = kint), intent(in) :: numele, nnod_4_ele
      integer(kind = kint), intent(in) :: ie(numele,nnod_4_ele)
      real(kind = kreal), intent(in) :: xx(numnod,3)
!
      integer(kind = kint), intent(in)                                  &
     &                  :: node_on_sf(nnod_4_surf,nsurf_4_ele)
      integer(kind = kint), intent(in)                                  &
     &                  :: node_on_sf_n(nnod_4_surf,nsurf_4_ele)
!
      integer(kind = kint), intent(in) :: np_smp, num_surf_smp
      integer(kind = kint), intent(in)                                  &
     &            :: isurf_grp_smp_stack(0:num_surf_smp)
      integer(kind = kint), intent(in) :: num_surf, num_surf_bc
      integer(kind = kint), intent(in) :: surf_istack(0:num_surf)
      integer(kind = kint), intent(in) :: surf_item(2,num_surf_bc)
!
      real(kind=kreal), intent(inout)                                   &
     &      :: dxe_sf(num_surf_bc,4,nnod_4_surf)
!
      integer (kind = kint) :: igrp, iproc, id_sf
      integer (kind = kint) :: ist, ied, inum, iele, isf
      integer (kind = kint) :: k2, kk2, kk2_n, inod, inod_n, nsf
!
!
      do igrp = 1, num_surf
!
        nsf = surf_istack(igrp) - surf_istack(igrp-1)
        if (nsf.gt.0) then
!$omp parallel do &
!$omp& private(id_sf,ist,ied,inum,iele,isf,kk2,kk2_n,inod,inod_n)
          do iproc = 1, np_smp
            id_sf = np_smp*(igrp-1) + iproc
            ist = isurf_grp_smp_stack(id_sf-1)+1
            ied = isurf_grp_smp_stack(id_sf)
            do k2 = 1, nnod_4_surf
!
!cdir nodep
!voption, indep, vec
              do inum = ist, ied
!
                iele = surf_item(1,inum)
                isf =  surf_item(2,inum)
                kk2 =    node_on_sf(k2,isf)
                kk2_n =  node_on_sf_n(k2,isf)
                inod =   ie(iele,kk2)
                inod_n = ie(iele,kk2_n)
!
                dxe_sf(inum,1,k2) = xx(inod,1) - xx(inod_n,1)
                dxe_sf(inum,2,k2) = xx(inod,2) - xx(inod_n,2)
                dxe_sf(inum,3,k2) = xx(inod,3) - xx(inod_n,3)
                dxe_sf(inum,4,k2)                                       &
     &              = one / sqrt( (xx(inod,1)-xx(inod_n,1))**2          &
     &                          + (xx(inod,2)-xx(inod_n,2))**2          &
     &                          + (xx(inod,3)-xx(inod_n,3))**2 )
!
              end do
            end do
          end do
!$omp end parallel do
!
        end if
      end do
!
      end subroutine delta_x_2_each_surf_grp
!
! ----------------------------------------------------------------------
!
      end module position_of_each_surface
