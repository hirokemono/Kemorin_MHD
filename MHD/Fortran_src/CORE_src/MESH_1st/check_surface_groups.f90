!check_surface_groups.f90
!      module check_surface_groups
!
!      Written by H> Matsui on Aug., 2006
!
!!      subroutine check_center_of_surface_grp                          &
!!     &         (id_check, sf_grp, sf_grp_v)
!!      subroutine check_center_of_surface_grp_sph                      &
!!     &          (id_check, sf_grp, sf_grp_v)
!      subroutine check_surface_param_smp(txt, id_check, sf_grp)
!
!       subroutine check_norm_surface_grp(id_check)
!
      module check_surface_groups
!
      use m_precision
!
      implicit  none
!
!-----------------------------------------------------------------------
!
      contains
!
!-----------------------------------------------------------------------
!
      subroutine check_center_of_surface_grp                            &
     &         (id_check, sf_grp, sf_grp_v)
!
      use t_group_data
      use t_surface_group_geometry
!
      integer(kind = kint), intent(in) :: id_check
      type(surface_group_data), intent(in) :: sf_grp
      type(surface_group_geometry), intent(in) :: sf_grp_v
!
      integer(kind = kint) :: i_grp, ist, ied, inum
!
      write(id_check,*) ' inum, center of surface'
      do i_grp = 1, sf_grp%num_grp
        ist = sf_grp%istack_grp(i_grp-1) + 1
        ied = sf_grp%istack_grp(i_grp)
        do inum = ist, ied
          write(id_check,'(i16,1p3e23.12)') inum,                       &
     &              sf_grp_v%x_sf_grp(inum,1:3)
        end do
      end do
!
      end subroutine check_center_of_surface_grp
!
!-----------------------------------------------------------------------
!
      subroutine check_center_of_surface_grp_sph                        &
     &          (id_check, sf_grp, sf_grp_v)
!
      use t_group_data
      use t_surface_group_geometry
!
      integer(kind = kint), intent(in) :: id_check
      type(surface_group_data), intent(in) :: sf_grp
      type(surface_group_geometry), intent(in) :: sf_grp_v
!
      integer(kind = kint) :: i_grp, ist, ied, inum
!
      write(id_check,*) ' inum, center of surface'
      write(id_check,*) '          (r, theta, phi, cyl_r)'
      do i_grp = 1, sf_grp%num_grp
        ist = sf_grp%istack_grp(i_grp-1) + 1
        ied = sf_grp%istack_grp(i_grp)
        do inum = ist, ied
          write(id_check,'(i16,1p4e23.12)') inum,                       &
     &        sf_grp_v%r_sf_grp(inum), sf_grp_v%theta_sf_grp(inum),     &
     &        sf_grp_v%phi_sf_grp(inum), sf_grp_v%s_sf_grp(inum)
        end do
      end do
!
      end subroutine check_center_of_surface_grp_sph
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
       subroutine check_surface_param_smp(txt, id_check, sf_grp)
!
       use m_machine_parameter
       use t_group_data
       use m_surface_group_connect
!
       integer(kind = kint), intent(in) :: id_check
       character(*), intent(in) :: txt
      type(surface_group_data), intent(in) :: sf_grp
!
       integer(kind = kint) :: isurf, ist, ied
!
       write(50+id_check,*) txt
       write(50+id_check,*) 'surf_istack'
       write(50+id_check,*) sf_grp%istack_grp(0:sf_grp%num_grp)
       write(50+id_check,*) 'inod_stack_sf_grp'
       write(50+id_check,*)                                             &
     &     sf_grp_nod1%inod_stack_sf_grp(0:sf_grp%num_grp)
!
       write(50+id_check,*) 'isurf_grp_smp_stack'
       do isurf = 1, sf_grp%num_grp
         ist = np_smp*(isurf-1) + 1
         ied = np_smp*isurf
         write(50+id_check,*) isurf, sf_grp%istack_grp_smp(ist:ied)
       end do
      write(50+id_check,*) 'isurf_nod_smp_stack'
      call check_surf_nod_4_sheard_para                                 &
     &   (id_check, sf_grp%num_grp, sf_grp_nod1)
!
      end subroutine check_surface_param_smp
!
!-----------------------------------------------------------------------
!
      subroutine check_norm_surface_grp(id_check, sf_grp, sf_grp_v)
!
      use m_geometry_data
      use t_group_data
      use t_surface_group_geometry
!
      integer(kind = kint), intent(in) :: id_check
      type(surface_group_data), intent(in) :: sf_grp
      type(surface_group_geometry), intent(in) :: sf_grp_v
!
      integer(kind = kint) :: isurf, iele
!
!
       do isurf = 1, sf_grp%num_item
         iele = sf_grp%item_sf_grp(1,isurf)
         write(id_check,'(2i16, 1p6E25.15e3)') isurf, iele,             &
     &           sf_grp_v%vnorm_sf_grp(isurf,1:3),                      &
     &           sf_grp_v%area_sf_grp(isurf), volume_ele(iele)
       end do
!
       end subroutine check_norm_surface_grp
!
!-----------------------------------------------------------------------
!
      end module check_surface_groups
