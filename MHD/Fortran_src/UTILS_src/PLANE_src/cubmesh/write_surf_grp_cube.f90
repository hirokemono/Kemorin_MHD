!write_surf_grp_cube.f90
!     module write_surf_grp_cube
!
!     Written by H. Matsui
!     modified by H. Matsui on Aug., 2007
!
!!       subroutine write_surface_group(ndz, nx, ny, nz, kpe)
!
      module write_surf_grp_cube
!
      use m_precision
!
      use m_grp_data_cub_kemo
      use m_cube_files_data
!
      implicit none
!
! ----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
      subroutine write_surface_group(ndz, nx, ny, nz, kpe)
!
      use m_fem_mesh_labels
!
      integer(kind = kint), intent(in) :: ndz
      integer(kind = kint), intent(in) :: nx, ny, nz
      integer(kind = kint), intent(in) :: kpe
!
      integer(kind = kint) :: ibd
      integer(kind = kint) :: i, j, istart
!
      integer(kind=kint), parameter ::  isix = 6, ifive = 5
!
!
       write(l_out,'(a)', advance='NO') hd_fem_sfgrp()
       write(l_out,'(10i16)') sufgrptot
       write(l_out,'(10i16)') (index(i),i=1,sufgrptot)
!
       do ibd = 1, neib
!                                                 .. zmin_surf
         if (ibd .eq. 1) then
           write(l_out,'(a)'  )  'zmin_surf'
         else if (ibd.gt.1 .and. ibd.lt.10) then
           write(l_out,'(a5,i1)'  )  'zmin_',ibd
         else if (ibd.ge.10 .and. ibd.lt.100) then
           write(l_out,'(a5,i2)'  )  'zmin_',ibd
         end if
!
         if (kpe == 1) then 
           istart = (nx-1)*(ny-1)*(ibd-1)
           write(l_out,'(6i16)')                                        &
     &            ( ( (i+(j-1)*(nx-1)+istart), i=1,nx-1 ),j=1,ny-1)
           write(l_out,'(6i16)') (( ifive, i=1, nx-1 ),j=1,ny-1)
         else
           write(l_out,'(6i16)')
           write(l_out,'(6i16)')
         endif
!                                                 .. zmax_surf
         if (ibd .eq. 1) then
           write(l_out,'(a)'  )  'zmax_surf'
         else if (ibd.gt.1 .and. ibd.lt.10) then
           write(l_out,'(a5,i1)'  )  'zmax_',ibd
         else if (ibd.ge.10 .and. ibd.lt.100) then
           write(l_out,'(a5,i2)'  )  'zmax_',ibd
         end if
!
         if (kpe == ndz) then 
           istart = (nx-1)*(ny-1)*(nz-ibd-1)
           write(l_out,'(6i16)')                                        &
     &             ( ( (i+(j-1)*(nx-1)+istart),i=1,nx-1 ),j=1,ny-1)
           write(l_out,'(6i16)') (( isix, i=1, nx-1 ),j=1,ny-1)
         else
           write(l_out,'(6i16)')
           write(l_out,'(6i16)')
         end if
       end do
!
      end subroutine write_surface_group
!
! ----------------------------------------------------------------------
!
      end module write_surf_grp_cube
