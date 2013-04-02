!m_ele_material_property.f90
!     module m_ele_material_property
!
!> @brief coefficients for each element
!
!     Written by H. Matsui
!
!      subroutine init_ele_material_property
!
      module m_ele_material_property
!
      use m_precision
!
      implicit  none
!
!
!>     coeffeicient for viscous diffusion for each element
      real  (kind=kreal), allocatable :: ak_d_velo(:)
!>     coeffeicient for thermal diffusion for each element
      real  (kind=kreal), allocatable :: ak_d_temp(:)
!>     coeffeicient for magnetic diffusion for each element
      real  (kind=kreal), allocatable :: ak_d_magne(:)
!>     coeffeicient for chemical diffusion for each element
      real  (kind=kreal), allocatable :: ak_d_composit(:)
!
!
!>     coeffeicient for thermal buoyancy for each element
      real  (kind=kreal), allocatable :: ak_buo(:)
!>     coeffeicient for compositional buoyancy for each element
      real  (kind=kreal), allocatable :: ak_comp_buo(:)
!>     coeffeicient for Lorentz force for each element
!
!-----------------------------------------------------------------------
!
      contains
!
!-----------------------------------------------------------------------
!
      subroutine init_ele_material_property
!
      use m_machine_parameter
      use m_control_parameter
      use m_geometry_parameter
      use m_physical_property
!
!
!    For thermal
!
      if (iflag_t_evo_4_temp.ge.1) then
        allocate( ak_d_temp(numele) )
        ak_d_temp(1:numele) = coef_d_temp
      end if
!
!    For convection
!
      if (iflag_t_evo_4_velo.ge.1) then
        allocate( ak_d_velo(numele) )
        ak_d_velo(1:numele) = coef_d_velo
!
        if (iflag_4_gravity.gt.0 .or. iflag_4_filter_gravity.gt.0) then
          allocate ( ak_buo(numele) )
          ak_buo(1:numele) = coef_buo
        end if
!
        if ( iflag_4_composit_buo .gt. 0) then
          allocate ( ak_comp_buo(numele) )
          ak_comp_buo(1:numele) = coef_comp_buo
        end if
      end if
!
!   For Induction
!
      if (iflag_t_evo_4_magne.ge.1 .or. iflag_t_evo_4_vect_p.ge.1) then
        allocate ( ak_d_magne(numele) )
        ak_d_magne(1:numele) = coef_d_magne
      end if
!
!   For dummy scalar
!
      if (iflag_t_evo_4_composit.ge.1) then
        allocate ( ak_d_composit(numele) )
        ak_d_composit(1:numele) = coef_d_light
      end if
!
!  check
!
      if (iflag_debug .gt. 0) then
       if ( allocated(ak_d_velo) ) then
        write(*,*)' coefficient for viscosity:         ', ak_d_velo(1)
       end if
       if ( allocated(ak_d_temp) ) then
        write(*,*)' coefficient for thermal diffusion: ', ak_d_temp(1)
       end if
       if ( allocated(ak_d_magne) ) then
        write(*,*)' coefficient for magnetic diffusion:', ak_d_magne(1)
       end if
       if ( allocated(ak_d_composit) ) then
        write(*,*)' coefficient for chemical diffusion:',               &
      &            ak_d_composit(1)
       end if
!
       if ( allocated(ak_buo) ) then
        write(*,*)' coefficient for gravity:          ', ak_buo(1)
       end if
       if ( allocated(ak_comp_buo) ) then
        write(*,*)' coefficient for compositional buoyancy: ',          &
     &      ak_comp_buo(1)
       end if
      end if
!
      end subroutine init_ele_material_property
!
!-----------------------------------------------------------------------
!
      end module m_ele_material_property
