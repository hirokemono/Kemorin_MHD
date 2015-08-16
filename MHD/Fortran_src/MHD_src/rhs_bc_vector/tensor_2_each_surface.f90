!
!      module tensor_2_each_surface
!
!      Written by H. Matsui on Sep., 2005
!
!!      subroutine sym_tensor_2_each_surface(sf_grp, ngrp_sf, id_grp_sf,&
!!     &          nd, k2, i_tensor)
!!        Input:  d_nod(1,i_tensor), Output:  vect_sf
!!      subroutine asym_tensor_2_each_surface(sf_grp, ngrp_sf,          &
!!     &          id_grp_sf, nd, k2, i_tensor)
!!        Input:  d_nod(1,i_tensor), Output:  vect_sf
!!         type(surface_group_data), intent(in) :: sf_grp
!
      module tensor_2_each_surface
!
      use m_precision
      use m_constants
      use m_machine_parameter
!
      use m_geometry_data
      use m_phys_constants
      use m_int_surface_data
      use t_group_data
!
      implicit none
!
! ----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
      subroutine sym_tensor_2_each_surface(sf_grp, ngrp_sf, id_grp_sf,  &
     &          nd, k2, i_tensor)
!
      use m_node_phys_data
!
      type(surface_group_data), intent(in) :: sf_grp
      integer (kind = kint), intent(in) :: ngrp_sf
      integer (kind = kint), intent(in) :: id_grp_sf(ngrp_sf)
      integer (kind = kint), intent(in) :: nd, k2, i_tensor
!
      integer (kind = kint) :: i, igrp, iproc, id_sf, nsf
      integer (kind = kint) :: ist, ied, inum, iele, isf
      integer (kind = kint) :: kk2, inod
      integer (kind = kint) :: n1, n2, n3
!
!
       n1 = i_tensor + l_sim_t(1,nd)
       n2 = i_tensor + l_sim_t(2,nd)
       n3 = i_tensor + l_sim_t(3,nd)
!
      do i = 1, ngrp_sf
        igrp = id_grp_sf(i)
!
        nsf = sf_grp%istack_grp(igrp) - sf_grp%istack_grp(igrp-1)
        if (nsf.gt.0) then
!
!$omp parallel do                                                       &
!$omp& private(id_sf,ist,ied,inum,iele,isf,kk2,inod)
          do iproc = 1, np_smp
            id_sf = np_smp*(igrp-1) + iproc
            ist = sf_grp%istack_grp_smp(id_sf-1)+1
            ied = sf_grp%istack_grp_smp(id_sf)
!
!cdir nodep
!voption, indep, vec
             do inum = ist, ied
               iele = sf_grp%item_sf_grp(1,inum)
               isf =  sf_grp%item_sf_grp(2,inum)
               kk2 =    surf1%node_on_sf(k2,isf)
               inod =   ele1%ie(iele,kk2)
!
               vect_sf(inum,1) = d_nod(inod,n1)
               vect_sf(inum,2) = d_nod(inod,n2)
               vect_sf(inum,3) = d_nod(inod,n3)
            end do
          end do
!$omp end parallel do
!
        end if
      end do
!
      end subroutine sym_tensor_2_each_surface
!
! ----------------------------------------------------------------------
!
      subroutine asym_tensor_2_each_surface(sf_grp, ngrp_sf,            &
     &          id_grp_sf, nd, k2, i_tensor)
!
      use m_node_phys_data
!
      type(surface_group_data), intent(in) :: sf_grp
      integer (kind = kint), intent(in) :: ngrp_sf
      integer (kind = kint), intent(in) :: id_grp_sf(ngrp_sf)
      integer (kind = kint), intent(in) :: nd, k2, i_tensor
!
      integer (kind = kint) :: i, igrp, iproc, id_sf, nsf
      integer (kind = kint) :: ist, ied, inum, iele, isf
      integer (kind = kint) :: kk2, inod
      integer (kind = kint) :: n1, n2, n3
!
!
       n1 = i_tensor + l_sim_t(1,nd)
       n2 = i_tensor + l_sim_t(2,nd)
       n3 = i_tensor + l_sim_t(3,nd)
!
      do i = 1, ngrp_sf
        igrp = id_grp_sf(i)
!
        nsf = sf_grp%istack_grp(igrp) - sf_grp%istack_grp(igrp-1)
        if (nsf.gt.0) then
!
!$omp parallel do                                                       &
!$omp& private(id_sf,ist,ied,inum,iele,isf,kk2,inod)
          do iproc = 1, np_smp
            id_sf = np_smp*(igrp-1) + iproc
            ist = sf_grp%istack_grp_smp(id_sf-1)+1
            ied = sf_grp%istack_grp_smp(id_sf)
!
            if (nd .eq. 1 ) then
!cdir nodep
!voption, indep, vec
              do inum = ist, ied
                iele = sf_grp%item_sf_grp(1,inum)
                isf =  sf_grp%item_sf_grp(2,inum)
                kk2 =    surf1%node_on_sf(k2,isf)
                inod =   ele1%ie(iele,kk2)
!
                vect_sf(inum,1) =  zero
                vect_sf(inum,2) =  d_nod(inod,i_tensor  )
                vect_sf(inum,3) =  d_nod(inod,i_tensor+1)
              end do
            else if (nd .eq. 2) then
!cdir nodep
!voption, indep, vec
              do inum = ist, ied
                iele = sf_grp%item_sf_grp(1,inum)
                isf =  sf_grp%item_sf_grp(2,inum)
                kk2 =    surf1%node_on_sf(k2,isf)
                inod =   ele1%ie(iele,kk2)
!
                vect_sf(inum,1) = -d_nod(inod,i_tensor  )
                vect_sf(inum,2) =  zero
                vect_sf(inum,3) =  d_nod(inod,i_tensor+2)
!
              end do
            else if (nd .eq. 3) then
!cdir nodep
!voption, indep, vec
              do inum = ist, ied
!
                iele = sf_grp%item_sf_grp(1,inum)
                isf =  sf_grp%item_sf_grp(2,inum)
                kk2 =    surf1%node_on_sf(k2,isf)
                inod =   ele1%ie(iele,kk2)
!
                vect_sf(inum,1) = -d_nod(inod,i_tensor+1)
                vect_sf(inum,2) = -d_nod(inod,i_tensor+2)
                vect_sf(inum,3) = zero
!
              end do
            end if
          end do
!$omp end parallel do
!
        end if
      end do
!
      end subroutine asym_tensor_2_each_surface
!
! ----------------------------------------------------------------------
!
      end module tensor_2_each_surface
