!
!      module tensor_2_each_surface
!
!      Written by H. Matsui on Sep., 2005
!
!      subroutine sym_tensor_2_each_surface(ngrp_sf, id_grp_sf, nd,     &
!     &          k2, i_tensor)
!        Input:  d_nod(1,i_tensor), Output:  vect_sf
!      subroutine asym_tensor_2_each_surface(ngrp_sf, id_grp_sf, nd,    &
!     &          k2, i_tensor)
!        Input:  d_nod(1,i_tensor), Output:  vect_sf
!
      module tensor_2_each_surface
!
      use m_precision
!
      use m_machine_parameter
      use m_geometry_parameter
      use m_geometry_data
      use m_surface_group
      use m_phys_constants
      use m_int_surface_data
!
      implicit none
!
! ----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
      subroutine sym_tensor_2_each_surface(ngrp_sf, id_grp_sf, nd,      &
     &          k2, i_tensor)
!
      use m_node_phys_data
!
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
!cdir nodep
!voption, indep, vec
         do inum = ist, ied
!
           iele = surf_item(1,inum)
           isf =  surf_item(2,inum)
           kk2 =    node_on_sf(k2,isf)
           inod =   ie(iele,kk2)
!
           vect_sf(inum,1) = d_nod(inod,n1)
           vect_sf(inum,2) = d_nod(inod,n2)
           vect_sf(inum,3) = d_nod(inod,n3)
!
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
      subroutine asym_tensor_2_each_surface(ngrp_sf, id_grp_sf, nd,     &
     &          k2, i_tensor)
!
      use m_node_phys_data
!
      integer (kind = kint), intent(in) :: ngrp_sf
      integer (kind = kint), intent(in) :: id_grp_sf(ngrp_sf)
      integer (kind = kint), intent(in) :: nd, k2, i_tensor
!
      integer (kind = kint) :: i, igrp, iproc, id_sf, nsf
      integer (kind = kint) :: ist, ied, inum, iele, isf
      integer (kind = kint) :: kk2, inod
      integer (kind = kint) :: n1, n2, n3
      real(kind = kreal), parameter :: zero = 0.0d0
!
!
       n1 = i_tensor + l_sim_t(1,nd)
       n2 = i_tensor + l_sim_t(2,nd)
       n3 = i_tensor + l_sim_t(3,nd)
!
      do i = 1, ngrp_sf
       igrp = id_grp_sf(i)
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
         if (nd .eq. 1 ) then
!cdir nodep
!voption, indep, vec
          do inum = ist, ied
!
           iele = surf_item(1,inum)
           isf =  surf_item(2,inum)
           kk2 =    node_on_sf(k2,isf)
           inod =   ie(iele,kk2)
!
           vect_sf(inum,1) =  zero
           vect_sf(inum,2) =  d_nod(inod,i_tensor  )
           vect_sf(inum,3) =  d_nod(inod,i_tensor+1)
!
          end do
         else if (nd .eq. 2) then
!cdir nodep
!voption, indep, vec
          do inum = ist, ied
!
           iele = surf_item(1,inum)
           isf =  surf_item(2,inum)
           kk2 =    node_on_sf(k2,isf)
           inod =   ie(iele,kk2)
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
           iele = surf_item(1,inum)
           isf =  surf_item(2,inum)
           kk2 =    node_on_sf(k2,isf)
           inod =   ie(iele,kk2)
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
