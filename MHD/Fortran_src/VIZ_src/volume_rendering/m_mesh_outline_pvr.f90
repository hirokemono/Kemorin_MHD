!
!      module m_mesh_outline_pvr
!
!        programmed by H.Matsui on May. 2006
!
!      subroutine cal_mesh_outline_pvr(i_pvr, numnod, xx)
!      subroutine allocate_phys_data_4_pvr
!      subroutine deallocate_phys_data_4_pvr
!
      module m_mesh_outline_pvr
!
      use m_precision
!
      implicit  none
!
!
      real(kind = kreal), allocatable :: center_g(:,:)
      real(kind = kreal), allocatable :: rmax_g(:)
!
!
!!@n   minimum value: xx_minmax(1,ndir,i_pvr)
!!@n   maximum value: xx_minmax(2,ndir,i_pvr)
      real(kind = kreal), allocatable :: xx_minmax_g(:,:,:)
!
      real(kind = kreal), allocatable :: d_minmax_pvr(:,:)
!
      real(kind = kreal), allocatable :: xx_minmax_l(:,:,:)
      real(kind = kreal), allocatable :: xx_minmax_tbl(:,:,:,:)
      private :: xx_minmax_l, xx_minmax_tbl
!
! -----------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
      subroutine allocate_mesh_outline_pvr
!
      use m_control_params_4_pvr
!
      allocate( rmax_g(num_pvr) )
      allocate( center_g(3,num_pvr) )
      allocate( xx_minmax_g(2,3,num_pvr) )
      allocate( d_minmax_pvr(2,num_pvr) )
!
      xx_minmax_g(1,1:3,1:num_pvr) =  1.0d-30
      xx_minmax_g(2,1:3,1:num_pvr) = -1.0d-30
!
      center_g = 0.0d0
      rmax_g = 0.0d0
!
      end subroutine allocate_mesh_outline_pvr
!
! -----------------------------------------------------------------------
!
      subroutine deallocate_mesh_outline_pvr
!
!
      deallocate( rmax_g, center_g, xx_minmax_g )
!
      end subroutine deallocate_mesh_outline_pvr
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine cal_mesh_outline_pvr(i_pvr, numnod, xx)
!
      use calypso_mpi
      use m_constants
      use m_machine_parameter
      use m_control_params_4_pvr
!
      integer(kind = kint), intent(in) :: i_pvr
!
      integer(kind = kint), intent(in) :: numnod
      real(kind = kreal), intent(in) :: xx(numnod,3)
!
      integer(kind = kint) :: inod, ip
      real(kind = kreal) :: rmax_l, r_from_ct
!
!
      allocate( xx_minmax_l(2,3,nprocs) )
      allocate( xx_minmax_tbl(2,3,nprocs,num_pvr) )
      xx_minmax_l =     0.0d0
      xx_minmax_tbl =   0.0d0
!
      ip = my_rank + 1
!
      xx_minmax_l(1,1:3,ip) = xx(1,1:3)
      xx_minmax_l(2,1:3,ip) = xx(1,1:3)
      do inod = 2, numnod
        xx_minmax_l(1,1,ip) = min(xx_minmax_l(1,1,ip), xx(inod,1))
        xx_minmax_l(1,2,ip) = min(xx_minmax_l(1,2,ip), xx(inod,2))
        xx_minmax_l(1,3,ip) = min(xx_minmax_l(1,3,ip), xx(inod,3))
        xx_minmax_l(2,1,ip) = max(xx_minmax_l(2,1,ip), xx(inod,1))
        xx_minmax_l(2,2,ip) = max(xx_minmax_l(2,2,ip), xx(inod,2))
        xx_minmax_l(2,3,ip) = max(xx_minmax_l(2,3,ip), xx(inod,3))
      end do
!
      xx_minmax_tbl = 0.0d0
      call MPI_allREDUCE( xx_minmax_l(1,1,1),                           &
     &    xx_minmax_tbl(1,1,1,i_pvr), (isix*nprocs),                    &
     &    CALYPSO_REAL,  MPI_SUM, CALYPSO_COMM, ierr_MPI)
!
      xx_minmax_g(1,1:3,i_pvr) = xx_minmax_tbl(1,1:3,1,i_pvr)
      xx_minmax_g(2,1:3,i_pvr) = xx_minmax_tbl(2,1:3,1,i_pvr)
      do ip = 2, nprocs
        xx_minmax_g(1,1:3,i_pvr) = min(xx_minmax_g(1,1:3,i_pvr),        &
     &                                 xx_minmax_tbl(1,1:3,ip,i_pvr) )
        xx_minmax_g(2,1:3,i_pvr) = max(xx_minmax_g(1,1:3,i_pvr),        &
     &                                 xx_minmax_tbl(2,1:3,ip,i_pvr) )
      end do
!
      center_g(1:3,i_pvr)                                               &
     &   = (xx_minmax_g(1,1:3,i_pvr) + xx_minmax_g(2,1:3,i_pvr)) / two
!
!
      inod = 1
      rmax_l = sqrt( (xx(inod,1) - center_g(1,i_pvr))                   &
     &              *(xx(inod,1) - center_g(1,i_pvr))                   &
     &             + (xx(inod,2) - center_g(2,i_pvr))                   &
     &              *(xx(inod,2) - center_g(2,i_pvr))                   &
     &             + (xx(inod,3) - center_g(3,i_pvr))                   &
     &              *(xx(inod,3) - center_g(3,i_pvr)) )
!
!
!
      do inod = 2, numnod
        r_from_ct = sqrt( (xx(inod,1) - center_g(1,i_pvr))              &
     &                   *(xx(inod,1) - center_g(1,i_pvr))              &
     &                  + (xx(inod,2) - center_g(2,i_pvr))              &
     &                   *(xx(inod,2) - center_g(2,i_pvr))              &
     &                  + (xx(inod,3) - center_g(3,i_pvr))              &
     &                   *(xx(inod,3) - center_g(3,i_pvr)) )
        rmax_l = max(rmax_l, r_from_ct)
      end do
!
      call MPI_allREDUCE( rmax_l, rmax_g(i_pvr), ione,                  &
     &    CALYPSO_REAL,  MPI_MAX, CALYPSO_COMM, ierr_MPI)
!
      if (iflag_debug .gt. 0) then
        write(*,*) 'xx_min_g', xx_minmax_g(1,1:3,i_pvr)
        write(*,*) 'xx_max_g', xx_minmax_g(2,1:3,i_pvr)
        write(*,*) 'center_g', center_g(1:3,i_pvr)
        write(*,*) 'rmax_g', rmax_g(i_pvr)
      end if
!
      deallocate(xx_minmax_l, xx_minmax_tbl)
!
      end subroutine cal_mesh_outline_pvr
!
! ----------------------------------------------------------------------
!
      end module m_mesh_outline_pvr
