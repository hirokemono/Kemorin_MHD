!write_surf_grp_cube.f90
!     module write_surf_grp_cube
!
!     Written by H. Matsui
!     modified by H. Matsui on Aug., 2007
!
!       subroutine write_surface_group(kpe)
!       subroutine write_surface_group_b(kpe)
!
      module write_surf_grp_cube
!
      use m_precision
!
      use m_size_4_plane
      use m_size_of_cube
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
       subroutine write_surface_group(kpe)
!
       integer(kind = kint) :: kpe
!
       integer(kind = kint) :: ibd
       integer(kind = kint) :: i, j, istart
!
       integer(kind=kint), parameter ::  isix = 6, ifive = 5
!
!
            write(l_out,'( a )') '!'
            write(l_out,'(  a  )')                                      &
     &        '! 4.3 surface group'
            write(l_out,'(10i10)') sufgrptot
            write(l_out,'(10i10)') (index(i),i=1,sufgrptot)
!
!
            do ibd = 1, neib
!
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
                write(l_out,'(6i10)')                                   &
     &               ( ( (i+(j-1)*(nx-1)+istart), i=1,nx-1 ),j=1,ny-1)
                write(l_out,'(6i10)') (( ifive, i=1, nx-1 ),j=1,ny-1)
              else
                write(l_out,'(6i10)')
                write(l_out,'(6i10)')
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
                write(l_out,'(6i10)')                                   &
     &             ( ( (i+(j-1)*(nx-1)+istart),i=1,nx-1 ),j=1,ny-1)
                write(l_out,'(6i10)') (( isix, i=1, nx-1 ),j=1,ny-1)
              else
                write(l_out,'(6i10)')
                write(l_out,'(6i10)')
              end if
!
            end do
!
!
      end subroutine write_surface_group
!
! ----------------------------------------------------------------------
!
       subroutine write_surface_group_b(kpe)
!
       integer(kind = kint) :: kpe
!
       integer(kind = kint) :: ibd
       integer(kind = kint) :: i, j, istart
!
       integer(kind=kint ), parameter ::  isix = 6, ifive = 5
!
!
            write(l_out) sufgrptot
            write(l_out) (index(i),i=1,sufgrptot)
!
!
            do ibd = 1, neib
!
!                                                 .. zmin_surf
              if (ibd .eq. 1) then
                write(l_out)  'zmin_surf'
              else if (ibd.gt.1 .and. ibd.lt.10) then
                write(l_out)  'zmin_',ibd
              else if (ibd.ge.10 .and. ibd.lt.100) then
                write(l_out)  'zmin_',ibd
              end if
!
              if (kpe == 1) then 
                istart = (nx-1)*(ny-1)*(ibd-1)
                write(l_out)                                            &
     &               ( ( (i+(j-1)*(nx-1)+istart), i=1,nx-1 ),j=1,ny-1)
                write(l_out) (( ifive, i=1, nx-1 ),j=1,ny-1)
              else
                write(l_out)
                write(l_out)
              endif
!                                                 .. zmax_surf
              if (ibd .eq. 1) then
                write(l_out)  'zmax_surf'
              else if (ibd.gt.1 .and. ibd.lt.10) then
                write(l_out)  'zmax_',ibd
              else if (ibd.ge.10 .and. ibd.lt.100) then
                write(l_out)  'zmax_',ibd
              end if
!
              if (kpe == ndz) then 
                istart = (nx-1)*(ny-1)*(nz-ibd-1)
                write(l_out)                                            &
     &             ( ( (i+(j-1)*(nx-1)+istart),i=1,nx-1 ),j=1,ny-1)
                write(l_out) (( isix, i=1, nx-1 ),j=1,ny-1)
              else
                write(l_out)
                write(l_out)
              end if
!
            end do
!
!
      end subroutine write_surface_group_b
!
! ----------------------------------------------------------------------
!
      end module write_surf_grp_cube
