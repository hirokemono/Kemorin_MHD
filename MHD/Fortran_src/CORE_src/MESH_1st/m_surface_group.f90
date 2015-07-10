!m_surface_group.f90
!     module m_surface_group
!
!> @brief surface group data
!
!      written by H. Matsui
!
!      subroutine allocate_surface_data
!      subroutine clear_surface_data
!      subroutine deallocate_surface_data
!
!      subroutine allocate_surface_param_smp
!      subroutine deallocate_surface_param_smp
!
!      subroutine check_surf_4_sheard_para(my_rank)
!
      module m_surface_group
!
      use m_precision
      use t_group_data
!
      implicit  none
!
!>  Structure for surfacet group
      type(surface_group_data), save :: sf_grp1
!sf_grp1%num_item
!
!      integer(kind=kint) :: num_surf
!<      number of surface group
!      integer(kind=kint) :: sf_grp1%num_item
!<      total number of surface for surface group
!
      integer(kind=kint),   allocatable, target :: surf_istack(:)
!<      end address of each surface group
      integer(kind=kint),    allocatable, target :: surf_item(:,:)
!<      local surface ID for surface group
!<      surf_item(1,:):  local element ID
!<      surf_item(2,:):  surface ID for each element
!
      character(len=kchara), allocatable, target :: surf_name(:)
!<      surface group name
!
      integer( kind=kint )  ::  num_surf_smp
!<      number of surface group for SMP process
      integer( kind=kint ), allocatable :: isurf_grp_smp_stack(:)
!<      end address of each surface group for SMP process
      integer( kind=kint )  ::  max_sf_grp_4_smp
!<      maximum number of surface group for SMP process
!
!
!-----------------------------------------------------------------------
!
      contains
!
!-----------------------------------------------------------------------
!
      subroutine allocate_surface_data
!
       allocate(surf_istack(0:sf_grp1%num_grp))
       allocate(surf_name(sf_grp1%num_grp))
       allocate(surf_item(2,sf_grp1%num_item))
!
      call clear_surface_data
!
      end subroutine allocate_surface_data
!
!-----------------------------------------------------------------------
!
      subroutine clear_surface_data
!
       surf_istack=0
       surf_item=0
!
      end subroutine clear_surface_data
!
!-----------------------------------------------------------------------
!
      subroutine deallocate_surface_data
!
       deallocate(surf_istack)
       deallocate(surf_name)
       deallocate(surf_item)
!
      end subroutine deallocate_surface_data
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
       subroutine allocate_surface_param_smp
!
       allocate( isurf_grp_smp_stack(0:num_surf_smp))
       isurf_grp_smp_stack = 0
!
       end subroutine allocate_surface_param_smp
!
!-----------------------------------------------------------------------
!
       subroutine deallocate_surface_param_smp
!
!
       deallocate(isurf_grp_smp_stack)
!
       end subroutine deallocate_surface_param_smp
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine check_surf_4_sheard_para(my_rank)
!
!
      integer(kind = kint), intent(in) :: my_rank
!
       write(*,*) 'PE: ', my_rank, 'num_surf ', sf_grp1%num_grp
       write(*,*) 'PE: ', my_rank, 'num_surf_smp ', num_surf_smp
       write(*,*) 'PE: ', my_rank,                                      &
     &            'isurf_grp_smp_stack ', isurf_grp_smp_stack
!
      end subroutine check_surf_4_sheard_para
!
!-----------------------------------------------------------------------
!
      end module m_surface_group
