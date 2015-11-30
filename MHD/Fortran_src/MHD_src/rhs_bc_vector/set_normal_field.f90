!set_normal_field.f90
!--- module set_normal_field ----------
!
!    Boundary condition setting
!
!        programmed by H.Matsui and H.Okuda
!                                    on July 2000 (ver 1.1)
!        modified by H.Matsui on Nov. 2003
!        modified by H.Matsui on Sep. 2005
!
!!      subroutine set_normal_velocity(sf_grp, sf_grp_nod)
!!        type(surface_group_data), intent(in) :: sf_grp
!!        type(surface_node_grp_data), intent(in) :: sf_grp_nod
!
      module set_normal_field
!
      use m_precision
!
      use m_machine_parameter
!
      implicit none
!
      private :: set_normal_comp
!
!-----------------------------------------------------------------------
!
      contains
!
!-----------------------------------------------------------------------
!
      subroutine set_normal_velocity(sf_grp, sf_grp_nod)
!
      use m_node_phys_address
      use m_node_phys_data
      use m_surf_data_torque
      use t_group_data
      use t_surface_group_connect
!
      type(surface_group_data), intent(in) :: sf_grp
      type(surface_node_grp_data), intent(in) :: sf_grp_nod
!
!
      if (sf_bc1_norm_v%ngrp_sf_fix_fx .gt. 0) then
        call set_normal_comp(sf_grp%num_grp, sf_grp%num_grp_smp,        &
     &      sf_grp_nod%ntot_node_sf_grp, sf_grp_nod%inod_stack_sf_grp,  &
     &      sf_grp_nod%istack_surf_nod_smp, sf_grp_nod%inod_surf_grp,   &
     &      sf_grp_nod%surf_norm_nod,                                   &
     &      sf_bc1_norm_v%ngrp_sf_fix_fx,                               &
     &      sf_bc1_norm_v%nitem_sf_fix_fx,                              &
     &      sf_bc1_norm_v%id_grp_sf_fix_fx,                             &
     &      sf_bc1_norm_v%ist_ele_sf_fix_fx,                            &
     &      sf_bc1_norm_v%sf_apt_fix_fx,                                &
     &      nod_fld1%n_point, nod_fld1%ntot_phys, iphys%i_velo,         &
     &      nod_fld1%d_fld)
      end if
!
      end subroutine set_normal_velocity
!
!-----------------------------------------------------------------------
!
      subroutine set_normal_comp(num_surf, num_surf_smp,                &
     &         ntot_node_sf_grp, inod_stack_sf_grp,                     &
     &         isurf_nod_smp_stack, inod_surf_grp, surf_norm_nod,       &
     &         ngrp_sf, nnod_sf, id_grp_sf, ist_nod_sf, sf_apt,         &
     &         numnod, ncomp_nod, i_vect, d_nod)
!
      integer (kind = kint), intent(in) :: num_surf, num_surf_smp
      integer (kind = kint), intent(in) :: ntot_node_sf_grp
      integer (kind = kint), intent(in)                                 &
     &                      :: inod_stack_sf_grp(0:num_surf)
      integer (kind = kint), intent(in)                                 &
     &                      :: isurf_nod_smp_stack(0:num_surf_smp)
      integer (kind = kint), intent(in)                                 &
     &                      :: inod_surf_grp(ntot_node_sf_grp)
      real(kind=kreal), intent(in) :: surf_norm_nod(ntot_node_sf_grp,3)
!
      integer (kind = kint), intent(in) :: ngrp_sf, nnod_sf
      integer (kind = kint), intent(in) :: id_grp_sf(ngrp_sf)
      integer (kind = kint), intent(in) :: ist_nod_sf(0:ngrp_sf)
      real (kind=kreal), intent(in) :: sf_apt(nnod_sf)
!
      integer (kind = kint), intent(in) :: numnod, ncomp_nod, i_vect
      real(kind = kreal), intent(inout) :: d_nod(numnod,ncomp_nod)
!
      integer (kind = kint) :: i, igrp, nsf
      integer (kind = kint) :: iproc, id_sf
      integer (kind = kint) :: ist, ied, inum, inod, idat
!
      do i = 1, ngrp_sf
        igrp = id_grp_sf(i)
!
        nsf = inod_stack_sf_grp(igrp) - inod_stack_sf_grp(igrp-1)
        if (nsf.gt.0) then
!
!$omp parallel do private(id_sf,ist,ied,inum,inod,idat)
          do iproc = 1, np_smp
            id_sf = np_smp*(igrp-1) + iproc
            ist = isurf_nod_smp_stack(id_sf-1)+1
            ied = isurf_nod_smp_stack(id_sf)
!
!cdir nodep
!VOPTION INDEP, VEC
            do inum = ist, ied
              inod = inod_surf_grp(inum)
              idat = ist_nod_sf(i-1) + inum - inod_stack_sf_grp(igrp-1)
              d_nod(inod,i_vect  ) = sf_apt(idat)*surf_norm_nod(inum,1)
              d_nod(inod,i_vect+1) = sf_apt(idat)*surf_norm_nod(inum,2)
              d_nod(inod,i_vect+2) = sf_apt(idat)*surf_norm_nod(inum,3)
            end do
          end do
!$omp end parallel do
!
        end if
      end do
!
      end subroutine set_normal_comp
!
!-----------------------------------------------------------------------
!
      end module set_normal_field
