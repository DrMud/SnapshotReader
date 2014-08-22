MODULE snapshotreader
  ! read Gadget-format Snapshot

  ! STORE VARIABLES
  INTEGER :: nparticles ! number of particles in snapshot
  REAL*4, ALLOCATABLE :: allPartCoords(:,:) ! [xpos,ypos,zpos] for each particle (units Mpc/h)
  REAL*4, ALLOCATABLE :: allPartMasses(:) ! masses for each particle (units Msol/h)
  INTEGER, ALLOCATABLE :: allIDlist(:) ! ID's for each particle
  REAL*8, SAVE :: BoxSize ! Mpc/h
  REAL*8, SAVE :: OmegaM0,OmegaL0,hubb !dimensionless
  REAL*8, SAVE :: massArr(6)    ! masses are in 10**10 Msol/h


CONTAINS

  SUBROUTINE readHeader(snap,flag,z)
    !Read GADGET snapshot header only
    !WARNING: snapshots made using Chris Power's ICs generator are 
    !         different from the standard. These have box sizes and positions
    !         in units of Mpc/h (CP's ICs) rather than kpc/h (standard)
    !WARNING: Masses may also be in different units
    !         Chris Power's ICs generator writes masses in units of 10**10*Msol/h
    !         The standard units are....
    IMPLICIT NONE
    CHARACTER(len=*),INTENT(IN):: snap
    INTEGER,INTENT(IN) :: flag
    REAL*4, INTENT(OUT) :: z
    real*8 :: dz !double precision version
    REAL*8 :: dummyD
    INTEGER :: dummyIa(10)
    INTEGER :: narr(6)
    OPEN(44, FILE=TRIM(snap),form='unformatted')
    READ(44) narr,massArr,dummyD,dz,dummyIa,BoxSize,OmegaM0,OmegaL0,hubb
    CLOSE(44)
    SELECT CASE(flag)
    CASE(0)
       BoxSize = BoxSize/1000. ! convert to Mpc/h
    END SELECT
    nparticles = SUM(narr)
    z = real(dz)
  END SUBROUTINE readHeader



  SUBROUTINE readGADGET(snap,flag)
    ! read and store information on every particle in simulation
    !WARNING: snapshots made using Chris Power's ICs generator are 
    !         different from the standard. These have box sizes and positions
    !         in units of Mpc/h (CP's ICs) rather than kpc/h (standard)
    !WARNING: Masses may also be in different units
    !         Chris Power's ICs generator writes masses in units of 10**10*Msol/h
    !         The standard units are....
    IMPLICIT NONE
    INTEGER,INTENT(IN) :: flag
    CHARACTER(len=*),INTENT(IN)     :: snap
    INTEGER             :: i,j,k
    INTEGER             :: ntype(6), nMasses, ntypei
    REAL*4, ALLOCATABLE :: masses(:)
    logical :: fexist
    !check snapshot exists
    inquire(file=trim(snap) , exist=fexist)
    if(fexist.eqv..false.)then
       write(*,*) 'Cannot find ',trim(snap)
       stop
    end if
    WRITE(*,*) 'READING HEADER of ',trim(snap)
    WRITE(*,*) '--------------------'
1   FORMAT(I8,A,I1,T30,A,E12.5,A)
    OPEN(45, FILE=TRIM(snap),form='unformatted')
    READ(45) ntype
    ! check number of particles!
    IF(nparticles.NE.SUM(ntype)) STOP 'Confusion with # total particles'
    ! Calculate number of particle masses listed in the 'masses' block
    nMasses = 0
    DO i=0,5 ! particle type
       WRITE(*,1) ntype(i+1),' particles of type ',i,': mass =',&
            &     massArr(i+1)*1e10,' (Msol/h)'
       IF(massArr(i+1).EQ.0)THEN !if mass varies for this type
          nMasses = nMasses + ntype(i+1)
       END IF
    END DO
    WRITE(*,*) 'number of particles listed in mass block =',nMasses
    WRITE(*,*)
    IF(nMasses.GT.0)THEN
       ALLOCATE(masses(nMasses))
    END IF
    ! Positions are in Mpc/h (CP's ICs) or kpc/h (standard)
    READ(45) (allPartCoords(i,1),allPartCoords(i,2),allPartCoords(i,3), i=1,nparticles)
    SELECT CASE(flag)
    CASE(0)
       allPartCoords(:,1:3) = allPartCoords(:,1:3)/1000. ! convert to Mpc/h
    END SELECT
    write(*,*) 'Maximum position : ',maxval(allPartCoords)
    write(*,*) 'Minimum position : ',minval(allPartCoords)
    READ(45) ! no need to read in velocities
    READ(45) allIDlist
    IF(nMasses.GT.0)THEN
       READ(45) (masses(i), i=1,nMasses)
    END IF
    CLOSE(45)
    k=0 ! count through each particle in simulation
    j=0 ! count through particle masses listed in 'masses' block
    DO i=0,5 ! particle type
       ntypei = ntype(i+1) ! number of particles of that type
       IF(ntypei.GT.0)THEN
          IF(massArr(i+1).GT.0.) THEN ! all particles this mass
             allPartMasses(k+1:k+ntypei) = REAL(massArr(i+1))
             WRITE(*,*) 'particles of type',i,'have the same mass'
          ELSE ! particles of varying masses
             WRITE(*,*) 'particles of type',i,'have varying mass'
             allPartMasses(k+1:k+ntypei) = masses(j+1:j+ntypei)
             j = j + ntypei
          END IF
          k = k + ntypei
       END IF
    END DO
    !WARNING: masses are in 10**10 Msol/h (standard)
    !convert masses to units Msol/h
    allPartMasses = allPartMasses * 1e10
    write(*,*) 'most massive particle (Msol/h) = ',maxval(allPartMasses)
    IF(ALLOCATED(masses))THEN
       DEALLOCATE(masses)
    END IF
    WRITE(*,*)
    IF (.NOT.(k.EQ.nparticles)) STOP 'how many particles have been included?'
    IF (.NOT.(j.EQ.nmasses)) STOP 'how many particles have masses included?'
    write(*,*) 'Finished reading information from GADGET file'
  END SUBROUTINE readGADGET



END MODULE snapshotreader
